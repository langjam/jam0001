#ifndef RAEL_PARSER_H
#define REAL_PARSER_H

#include "lexer.h"

#include <stdbool.h>

enum ValueType {
    ValueTypeVoid,
    ValueTypeNumber,
    ValueTypeString
};

struct Value {
    enum ValueType type;
    union {
        char *string;
        int number;
    } value;
};

struct Expr {
    bool raw;
    union {
        struct Value *raw;
        char *key;
    } as;
};

enum NodeType {
    NodeTypeLog = 1,
    NodeTypeSet,
};

struct Node {
    enum NodeType type;
    union {
        struct Expr *log_value;
        struct {
            char *key;
            struct Expr *expr;    
        } set;
    } value;
};

struct Parser {
    struct Lexer lexer;
    struct Node** nodes;
    size_t idx, allocated;
};

struct Node **parse(char* const stream);

#endif // RAEL_PARSE_H
