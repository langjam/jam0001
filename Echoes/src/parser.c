#include "parser.h"
#include "lexer.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef enum Precedence {
    PrecedenceLowest = 1,
    PrecedenceSum,
    PrecedenceProduct
} Precedence;

static inline bool parser_error(const struct Parser* const parser, const char* const error_message) {
    printf("ParserError: %s. on line: %ld, column: %ld\n",
            error_message, parser->lexer.line, parser->lexer.column);
    exit(1);
}

static void parser_push(struct Parser* const parser, struct Node* const node) {
    if (parser->allocated == 0) {
        parser->nodes = malloc((parser->allocated = 32)+1 * sizeof(struct Node*));
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
    if (!lexer_tokenize(&parser->lexer))
        return NULL;
    switch (parser->lexer.token.name) {
    case TokenNameNumber: {
        int number = 0;
        expr = malloc(sizeof(struct Expr));
        for (size_t i = 0; i < parser->lexer.token.length; ++i) {
            number *= 10;
            number += parser->lexer.token.string[i] - '0';
        }
        expr->type = ExprTypeValue;
        expr->as.value = malloc(sizeof(struct Value));
        expr->as.value->type = ValueTypeNumber;
        expr->as.value->value.number = number;
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
        expr->as.value->value.string = string;
        return expr;
    }
    case TokenNameKey: {
        char *key;
        expr = malloc(sizeof(struct Expr));
        key = malloc((parser->lexer.token.length+1) * sizeof(char));
        strncpy(key, parser->lexer.token.string, parser->lexer.token.length);
        key[parser->lexer.token.length] = '\0';
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
        case ExprTypeDiv: {
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
    // allocate the key
    key = malloc((key_token.length + 1) * sizeof(char));
    strncpy(key, key_token.string, key_token.length);
    key[key_token.length] = '\0';
    node->value.set.key = key;
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
    while (true) {
        if (!(node = parser_parse_node_log(&parser)) && !(node = parser_parse_node_set(&parser))) {
            if (lexer_tokenize(&parser.lexer)) {
                parser_error(&parser, "Syntax Error");
            } else {
                break;
            }
        }
        parser_push(&parser, node);
    }
    return parser.nodes;
}
