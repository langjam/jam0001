#include "parser.h"
#include "lexer.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static struct Expr *parser_parse_expr(struct Parser* const parser) {
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
        expr->as.raw = malloc(sizeof(struct Value));
        expr->as.raw->type = ValueTypeNumber;
        expr->as.raw->value.number = number;
        return expr;
    }
    case TokenNameString: {
        char *string;
        expr = malloc(sizeof(struct Expr));
        string = malloc((parser->lexer.token.length + 1) * sizeof(char));
        strncpy(string, parser->lexer.token.string, parser->lexer.token.length);
        string[parser->lexer.token.length] = '\0';
        expr->as.raw = malloc(sizeof(struct Value));
        expr->as.raw->type = ValueTypeString;
        expr->as.raw->value.string = string;
        return expr;
    }
    case TokenNameKey: {
        char *key;
        expr = malloc(sizeof(struct Expr));
        key = malloc((parser->lexer.token.length+1) * sizeof(char));
        strncpy(key, parser->lexer.token.string, parser->lexer.token.length);
        key[parser->lexer.token.length] = '\0';
        expr->as.key = key;
        return expr;
    }
    default:
        parser->lexer = old_state;
        return NULL;
    }
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
