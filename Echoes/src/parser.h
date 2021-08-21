#ifndef ECHOES_PARSER_H
#define ECHOES_PARSER_H

#include "lexer.h"

#include <stdbool.h>

enum ValueType {
    ValueTypeVoid,
    ValueTypeNumber,
    ValueTypeString,
    ValueTypeRoutine
};

struct RoutineValue {
    char **parameters;
    size_t amount_parameters;
    struct Node **block;
};

struct Value {
    enum ValueType type;
    union {
        char *string;
        int number;
        struct RoutineValue routine;
    } as;
};

enum ExprType {
    ExprTypeValue = 1,
    ExprTypeKey,
    ExprTypeAdd,
    ExprTypeSub,
    ExprTypeMul,
    ExprTypeDiv
};

struct Expr {
    enum ExprType type;
    union {
        struct {
            struct Expr *lhs, *rhs;
        } binary;
        struct Value *value;
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

#endif // ECHOES_PARSER_H
