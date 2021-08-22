#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include <stdbool.h>

extern int yylex(void);
extern int yyparse(void);
extern FILE *yyin;
extern bool did_error;

void yyerror(const char*);

typedef enum _DataType
{
    DATA_TYPE_NONE,
    DATA_TYPE_NUMBER,
    DATA_TYPE_STRING,
    DATA_TYPE_LIST,
} DataType;

typedef enum _ValueType
{
    VALUE_TYPE_NONE,
    VALUE_TYPE_NUMBER,
    VALUE_TYPE_BOOL,
    VALUE_TYPE_STRING,
} ValueType;

typedef struct _Value
{
    ValueType type;

    double num;
    int bool_val;
    const char *str;
} Value;

typedef enum _OperatorType
{
    OPERATOR_VALUE,
    OPERATOR_LIST,
    OPERATOR_INDEX,
    OPERATOR_ADD,
    OPERATOR_SUBTRACT,
    OPERATOR_MULTIPLY,
    OPERATOR_DIVIDE,
} OperatorType;

typedef struct _Expression
{
    int op;

    struct _Expression *lhs;
    struct _Expression *rhs;

    struct _List *list;
    Value value;
} Expression;

typedef enum _Comparitor
{
    COMPARITOR_LESS_THAN_OR_EQUAL,
    COMPARITOR_MORE_THAN_OR_EQUAL,
    COMPARITOR_LESS_THAN,
    COMPARITOR_MORE_THAN,
    COMPARITOR_EQUALS,
} Comparitor;

typedef struct _MatchConditionList
{
    Comparitor comparitor;
    Expression *value;
    struct _Body *body;
    struct _MatchConditionList *next;
} MatchConditionList;

typedef enum _PipelineType
{
    PIPELINE_TYPE_CALL,
    PIPELINE_TYPE_MATCH,
    PIPELINE_TYPE_EVAL,
    PIPELINE_TYPE_END,
} PipelineType;

typedef struct _Pipeline
{
    PipelineType type;
    MatchConditionList *condition_list;
    Expression *expression;
    double function;
    double out;
    struct _Pipeline *next;
} Pipeline;

typedef enum _StatementType
{
    STATEMENT_TYPE_FUNCTION,
    STATEMENT_TYPE_PIPELINE,
} StatementType;

typedef struct _Statement
{
    StatementType type;
    Expression *in;
    Pipeline *pipeline;
    struct _Function *function;
} Statement;

typedef struct _Function
{
    double name;
    DataType parameter_type;
    DataType return_type;
    struct _Body *body;
} Function;

typedef struct _Body
{
    Statement *statement;
    struct _Body *next;
} Body;

typedef struct _List
{
    Expression *value;
    struct _List *next;
} List;

extern Body *parsed_program;
void debug_log_program(Body*);

#endif // PARSER_H

