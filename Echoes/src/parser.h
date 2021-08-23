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

struct NumberExpr {
    bool is_float;
    union {
        int _int;
        double _float;
    } as;
};

struct Value {
    enum ValueType type;
    union {
        char *string;
        struct NumberExpr number;
        struct RoutineValue routine;
    } as;
};

enum ExprType {
    ExprTypeValue = 1,
    ExprTypeKey,
    ExprTypeAdd,
    ExprTypeSub,
    ExprTypeMul,
    ExprTypeDiv,
    ExprTypeEquals,
    ExprTypeSmallerThen,
    ExprTypeBiggerThen
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
    NodeTypeIf,
    NodeTypeLoop,
};

struct IfStatementNode {
    struct Expr *condition;
    struct Node **block;
    struct Node **else_block;
};

struct LoopNode {
    struct Expr *condition;
    struct Node **block;
};

struct Node {
    enum NodeType type;
    union {
        struct Expr *log_value;
        struct {
            char *key;
            struct Expr *expr;    
        } set;
        struct IfStatementNode if_stat;
        struct LoopNode loop;
    } value;
};

struct Parser {
    struct Lexer lexer;
    struct Node** nodes;
    size_t idx, allocated;
};

struct Node **parse(char* const stream);

#endif // ECHOES_PARSER_H
