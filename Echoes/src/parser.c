#include "parser.h"
#include "lexer.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

typedef enum Precedence {
    PrecedenceLowest = 1,
    PrecedenceComparison,
    PrecedenceSum,
    PrecedenceProduct
} Precedence;

static struct Node *parser_parse_node(struct Parser* const parser);
static struct Value *parser_parse_node_routine(struct Parser* const parser);

static inline bool parser_error(const struct Parser* const parser, const char* const error_message) {
    printf("ParserError: %s. on line: %ld, column: %ld\n",
            error_message, parser->lexer.line, parser->lexer.column);
    exit(1);
}

static void parser_push(struct Parser* const parser, struct Node* const node) {
    if (parser->allocated == 0) {
        parser->nodes = malloc(((parser->allocated = 32)+1) * sizeof(struct Node*));
    } else if (parser->idx == 32) {
        parser->nodes = realloc(parser->nodes, (parser->allocated += 32)+1 * sizeof(struct Node*));
    }
    parser->nodes[parser->idx++] = node;
}

static void parser_expect_newline(struct Parser* const parser) {
    if (lexer_tokenize(&parser->lexer)) {
        if (parser->lexer.token.name != TokenNameNewline) {
            parser_error(parser, "Expected newline after expression");
        }
    }
}

static struct Expr *parser_parse_literal_expr(struct Parser* const parser) {
    struct Expr *expr;
    struct Lexer old_state = parser->lexer;
    struct Value *routine;
    if ((routine = parser_parse_node_routine(parser))) {
        expr = malloc(sizeof(struct Expr));
        expr->type = ExprTypeValue;
        expr->as.value = routine; 
        return expr;
    }
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    switch (parser->lexer.token.name) {
    case TokenNameNumber: {
        struct NumberExpr number = {
            .is_float = false,
        };
        double as_float = 0.f;
        int as_int = 0;
        expr = malloc(sizeof(struct Expr));
        for (size_t i = 0; i < parser->lexer.token.length; ++i) {
            if (parser->lexer.token.string[i] == '.') {
                if (!number.is_float) {
                    number.is_float = true;
                    continue;
                } else {
                    parser_error(parser, "Unexpected '.' after float literal");
                }
            }
            if (!number.is_float) {
                as_int *= 10;
                as_int += parser->lexer.token.string[i] - '0';
            }
            as_float *= 10;
            as_float += parser->lexer.token.string[i] - '0';
            if (number.is_float)
                as_float /= 10;
        }
        if (number.is_float)
            number.as._float = as_float;
        else
            number.as._int = as_int;
        expr->type = ExprTypeValue;
        expr->as.value = malloc(sizeof(struct Value));
        expr->as.value->type = ValueTypeNumber;
        expr->as.value->as.number = number;
        return expr;
    }
    case TokenNameString: {
        char *string;
        expr = malloc(sizeof(struct Expr));
        string = malloc((parser->lexer.token.length + 1) * sizeof(char));
        strncpy(string, parser->lexer.token.string, parser->lexer.token.length);
        string[parser->lexer.token.length] = '\0';
        expr->type = ExprTypeValue;
        expr->as.value = malloc(sizeof(struct Value));
        expr->as.value->type = ValueTypeString;
        expr->as.value->as.string = string;
        return expr;
    }
    case TokenNameKey: {
        char *key;
        expr = malloc(sizeof(struct Expr));
        key = token_allocate_key(&parser->lexer.token);
        expr->as.key = key;
        expr->type = ExprTypeKey;
        return expr;
    }
    default:
        parser->lexer = old_state;
        return NULL;
    }
}


static enum ExprType token_name_to_expr_type(const enum ExprType type) {
    switch (type) {
    case TokenNameEquals:
        return ExprTypeEquals;
    case TokenNameSmallerThan:
        return ExprTypeSmallerThen;
    case TokenNameBiggerThan:
        return ExprTypeBiggerThen;
    case TokenNameAdd:
        return ExprTypeAdd;
    case TokenNameSub:
        return ExprTypeSub;
    case TokenNameMul:
        return ExprTypeMul;
    case TokenNameDiv:
        return ExprTypeDiv;
    default:
        assert(0);
    }
}

static Precedence token_name_to_precedence(const enum TokenName name) {
    switch (name) {
    case TokenNameEquals:
    case TokenNameSmallerThan:
    case TokenNameBiggerThan:
        return PrecedenceComparison;
    case TokenNameAdd:
    case TokenNameSub:
        return PrecedenceSum;
    case TokenNameMul:
    case TokenNameDiv:
        return PrecedenceProduct;
    case TokenNameLeftParen:
        return PrecedenceLowest;
    default:
        assert(0);
    }
}

static struct Expr *parser_parse_expr(struct Parser* const parser) {
    struct Token operator_stack[256];
    struct Expr* output_stack[256];
    struct Expr* expr_stack[256];
    size_t op_idx = 0, out_idx = 0, expr_idx = 0;

    while (true) {
        struct Lexer last_state = parser->lexer;
        if ((output_stack[out_idx] = parser_parse_literal_expr(parser))) {
            ++out_idx;
            continue;
        }
        if (!lexer_tokenize(&parser->lexer))
            break;
        switch (parser->lexer.token.name) {
        case TokenNameLeftParen:
            operator_stack[op_idx++] = parser->lexer.token;
            continue;
        case TokenNameRightParen:
            while (operator_stack[--op_idx].name != TokenNameLeftParen) {
                output_stack[out_idx] = malloc(sizeof(struct Expr));
                output_stack[out_idx]->type = token_name_to_expr_type(operator_stack[op_idx].name);
                ++out_idx;
            }
            continue;
        case TokenNameAdd:
        case TokenNameSub:
        case TokenNameMul:
        case TokenNameDiv:
        case TokenNameEquals:
        case TokenNameSmallerThan:
        case TokenNameBiggerThan:
            if (op_idx > 0 &&
                    token_name_to_precedence(parser->lexer.token.name) <= token_name_to_precedence(operator_stack[op_idx-1].name)) {
                // pop operations into output_stack
                output_stack[out_idx] = malloc(sizeof(struct Expr));
                output_stack[out_idx]->type = token_name_to_expr_type(operator_stack[--op_idx].name);
                ++out_idx;
            }
            // push operator to stack
            operator_stack[op_idx++] = parser->lexer.token;
            continue;
        default:
            parser->lexer = last_state;
        }
        break;
    }
    // pop remaining into output_stack
    for (size_t i = 1; i <= op_idx; ++i) {
        output_stack[out_idx] = malloc(sizeof(struct Expr));
        output_stack[out_idx]->type = token_name_to_expr_type(operator_stack[op_idx-i].name);
        ++out_idx;
    }
    for (size_t i = 0; i < out_idx; ++i) {
        switch (output_stack[i]->type) {
        case ExprTypeAdd:
        case ExprTypeSub:
        case ExprTypeMul:
        case ExprTypeDiv:
        case ExprTypeEquals:
        case ExprTypeSmallerThen:
        case ExprTypeBiggerThen: {
            struct Expr *expr;
            if (expr_idx < 2) {
                parser_error(parser, "Expected operator between operands");
            }
            expr = output_stack[i];
            expr->as.binary.lhs = expr_stack[expr_idx-2];
            expr->as.binary.rhs = expr_stack[expr_idx-1];
            expr_stack[expr_idx -= 2] = expr;
            ++expr_idx;
            break;
        }
        default:
            expr_stack[expr_idx++] = output_stack[i];
            break;
        }
    }
    if (expr_idx != 1) {
        parser_error(parser, "Expected operator between operands");
    }
    return expr_stack[0];
}

static struct Node *parser_parse_node_set(struct Parser* const parser) {
    struct Lexer old_state = parser->lexer;
    struct Node *node;
    struct Expr *expr;
    char *key;
    struct Token key_token;
    // is there nothing?
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    // doesn't start with "log"
    if (parser->lexer.token.name != TokenNameKey) {
        parser->lexer = old_state;
        return NULL;
    }
    key_token = parser->lexer.token;
    // expect an expression after "log"
    if (!(expr = parser_parse_expr(parser))) {
        parser_error(parser, "Expected an expression after \"log\"");
    }
    parser_expect_newline(parser);
    node = malloc(sizeof(struct Node));
    node->type = NodeTypeSet;
    node->value.set.key = token_allocate_key(&key_token);
    node->value.set.expr = expr;
    return node;
}

static struct Node *parser_parse_node_log(struct Parser* const parser) {
    struct Lexer old_state = parser->lexer;
    struct Node *node;
    struct Expr *expr;
    // is there nothing?
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    // doesn't start with "log"
    if (parser->lexer.token.name != TokenNameLog) {
        parser->lexer = old_state;
        return NULL;
    }
    // expect an expression after "log"
    if (!(expr = parser_parse_expr(parser))) {
        parser_error(parser, "Expected an expression after \"log\"");
    }
    parser_expect_newline(parser);
    node = malloc(sizeof(struct Node));
    node->type = NodeTypeLog;
    node->value.log_value = expr;
    return node;
}

static struct Node **parser_parse_block(struct Parser* const parser) {
    struct Lexer old_state = parser->lexer;
    struct Node **nodes;
    size_t node_amount, node_idx = 0;
    // is there something?
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    // verify it starts with '{'
    if (parser->lexer.token.name != TokenNameLeftCur) {
        parser->lexer = old_state;
        return NULL;
    }
    parser_expect_newline(parser);
    nodes = malloc(((node_amount = 32)+1) * sizeof(struct Node *));
    while (true) {
        struct Node *node;
        if (!(node = parser_parse_node(parser))) {
            if (!lexer_tokenize(&parser->lexer))
                parser_error(parser, "Expected '}'");
            if (parser->lexer.token.name == TokenNameRightCur) {
                break;
            } else {
                parser_error(parser, "Unexpected token");
            }
        }
        if (node_idx == node_amount) {
            nodes = realloc(nodes, ((node_amount += 32)+1) * sizeof(struct Node *));
        }
        nodes[node_idx++] = node;
    }
    nodes[node_idx] = NULL;
    return nodes;
}

static struct Value *parser_parse_node_routine(struct Parser* const parser) {
    struct Value *value;
    struct RoutineValue decl;
    struct Lexer old_state = parser->lexer;
    // is there something?
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    // verify it starts with 'routine'
    if (parser->lexer.token.name != TokenNameRoutine) {
        parser->lexer = old_state;
        return NULL;
    }
    // verify there is '(' after 'routine'?
    if (!lexer_tokenize(&parser->lexer) || parser->lexer.token.name != TokenNameLeftParen) {
        parser->lexer = old_state;
        parser_error(parser, "Expected '(' after 'routine'");
    }
    // verify you can lex and lex
    if (!lexer_tokenize(&parser->lexer))
        parser_error(parser, "Unexpected EOF");
    // if there is a ')', it is the end
    if (parser->lexer.token.name == TokenNameRightParen) {
        decl.amount_parameters = 0;
        decl.parameters = NULL;
    } else if (parser->lexer.token.name == TokenNameKey) {
        char *key = token_allocate_key(&parser->lexer.token);
        size_t allocated;
        decl.parameters = malloc((allocated = 4) * sizeof(char *));
        decl.amount_parameters = 0;
        decl.parameters[decl.amount_parameters++] = key;
        while (true) {
            // verify you can tokenize
            if (!lexer_tokenize(&parser->lexer))
                parser_error(parser, "Unexpected EOF");
            // if the token is ')', you can exit the loop
            if (parser->lexer.token.name == TokenNameRightParen)
                break;
            // if there is no comma, error
            if (parser->lexer.token.name != TokenNameComma)
                parser_error(parser, "Expected Comma");
            // verify you can lex
            if (!lexer_tokenize(&parser->lexer))
                parser_error(parser, "Unexpected EOF");
            // verify the token is a key
            if (parser->lexer.token.name != TokenNameKey)
                parser_error(parser, "Expected key");
            // allocate the key
            key = token_allocate_key(&parser->lexer.token);
            // reallocate if needed
            if (decl.amount_parameters == allocated) {
                decl.parameters = realloc(decl.parameters, (allocated += 3)*sizeof(char *));
            }
            // push parameter
            decl.parameters[decl.amount_parameters++] = key;
        }
    } else {
        parser_error(parser, "Unexpected token");
    }
    if (!(decl.block = parser_parse_block(parser))) {
        parser_error(parser, "Expected block after routine decleration");
    }
    value = malloc(sizeof(struct Value));
    value->type = ValueTypeRoutine;
    value->as.routine = decl;
    return value;
}

static struct Node *parser_parse_if_statement(struct Parser* const parser) {
    struct Node *node;
    struct Lexer old_state = parser->lexer;
    struct IfStatementNode if_stat = {
        .block = NULL,
        .else_block = NULL,
        .condition = NULL
    };
    // is there something?
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    // verify it starts with 'if'
    if (parser->lexer.token.name != TokenNameIf) {
        parser->lexer = old_state;
        return NULL;
    }
    if (!(if_stat.condition = parser_parse_expr(parser))) {
        parser_error(parser, "Expected an expression after if keyword");
    }
    if (!(if_stat.block = parser_parse_block(parser))) {
        parser_error(parser, "Expected block after if (expr)");
    }
    node = malloc(sizeof(struct Node));
    node->type = NodeTypeIf;
    old_state = parser->lexer;
    if (!lexer_tokenize(&parser->lexer)) {
        goto end;
    }
    if (parser->lexer.token.name != TokenNameElse) {
        parser->lexer = old_state;
        goto end;
    }
    if (!(if_stat.else_block = parser_parse_block(parser))) {
        parser_error(parser, "Expected block after Else keyword");
    }
end:
    node->value.if_stat = if_stat;
    return node;
}

static struct Node *parser_parse_loop(struct Parser* const parser) {
    struct Node *node;
    struct Lexer old_state = parser->lexer;
    struct LoopNode loop = {
        .block = NULL,
        .condition = NULL
    };
    // is there something?
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    // verify it starts with 'loop'
    if (parser->lexer.token.name != TokenNameLoop) {
        parser->lexer = old_state;
        return NULL;
    }
    if (!(loop.condition = parser_parse_expr(parser))) {
        parser_error(parser, "Expected an expression after if keyword");
    }
    if (!(loop.block = parser_parse_block(parser))) {
        parser_error(parser, "Expected block after expression");
    }
    parser_expect_newline(parser);
    node = malloc(sizeof(struct Node));
    node->type = NodeTypeLoop;
    node->value.loop = loop;
    return node;
}

static struct Node *parser_parse_node(struct Parser* const parser) {
    struct Node *node;
    if ((node = parser_parse_node_set(parser))     ||
        (node = parser_parse_node_log(parser))     ||
        (node = parser_parse_if_statement(parser)) ||
        (node = parser_parse_loop(parser))) {
        return node;
    }
    return NULL;
}

struct Node **parse(char* const stream) {
    struct Node *node;
    struct Parser parser = {
        .allocated = 0,
        .idx = 0,
        .nodes = NULL,
        .lexer = {
            .line = 1,
            .column = 1,
            .stream = stream
        }
    };
    while ((node = parser_parse_node(&parser))) {
        parser_push(&parser, node);
    }
    if (lexer_tokenize(&parser.lexer)) {
        parser_error(&parser, "Syntax Error");
    }
    return parser.nodes;
}
