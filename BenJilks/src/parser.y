%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parser.h"
#include "allocator.h"

Body *parsed_program = NULL;
extern int yylineno;

%}

%union
{
    DataType datatype;
    OperatorType op;
    Comparitor comparitor;
    Value value;
    double number;

    Function *function;
    Body *body;
    Statement *statement;
    Pipeline *pipeline;
    MatchConditionList *condition_list;
    Expression *expression;
    List *list;
}

%token<datatype>        TOKEN_DATA_TYPE
%token<number>          TOKEN_NUMBER
%token<value>           TOKEN_VALUE
%token<op>              TOKEN_OPERATOR
%token<comparitor>      TOKEN_COMPARITOR

%token                  TOKEN_ARROW         "->"
%token                  TOKEN_OPEN_SQUIGLE  "{"
%token                  TOKEN_CLOSE_SQUIGLE "}"
%token                  TOKEN_OPEN_SQUARE   "["
%token                  TOKEN_CLOSE_SQUARE  "]"
%token                  TOKEN_OPEN_BRACE    "("
%token                  TOKEN_CLOSE_BRACE   ")"
%token                  TOKEN_COMMA         ","
%token                  TOKEN_COLON         ":"
%token                  TOKEN_MATCH         "match"
%token                  TOKEN_EVAL          "eval"
%token                  TOKEN_DEFINE        "define"
%token                  TOKEN_PLEASE        "please"
%token                  TOKEN_THANKS        "thanks"

%type<body>             body
%type<statement>        statement
%type<function>         function
%type<pipeline>         pipeline
%type<condition_list>   match_statement
%type<condition_list>   match_condition_list
%type<expression>       expression
%type<list>             list
%type<value>            value

%left                   TOKEN_OPERATOR
%left                   TOKEN_OPEN_SQUARE

%start body
%locations

%%

body:
    statement body
    {
        $$ = ALLOCATE_TYPE(Body);
        $$->statement = $1;
        $$->next = $2;
        parsed_program = $$;
    }
|   statement error
    {
        printf("Error: Invalid statement\n");
    }
|   %empty
    {
        $$ = NULL;
    }

statement:
    expression "->" pipeline
    {
        $$ = ALLOCATE_TYPE(Statement);
        $$->type = STATEMENT_TYPE_PIPELINE;
        $$->in = $1;
        $$->pipeline = $3;
    }
|   function
    {
        $$ = ALLOCATE_TYPE(Statement);
        $$->type = STATEMENT_TYPE_FUNCTION;
        $$->function = $1;
    }
|   expression "->" error
    {
        printf("Error: Invalid pipeline entry\n");
    }

function:
    "please" "define" TOKEN_DATA_TYPE "->" TOKEN_NUMBER "->" TOKEN_DATA_TYPE "{" body "}" "thanks"
    {
        $$ = ALLOCATE_TYPE(Function);
        $$->parameter_type = $3;
        $$->name = $5;
        $$->return_type = $7;
        $$->body = $9;
    }
|   error "define" TOKEN_DATA_TYPE "->" TOKEN_NUMBER "->" TOKEN_DATA_TYPE "{" body "}" "thanks"
    {
        printf("Error: Watch your manners!\n");
    }
|   "please" "define" TOKEN_DATA_TYPE "->" TOKEN_NUMBER "->" TOKEN_DATA_TYPE "{" body "}" error
    {
        printf("Error: Watch your manners!\n");
    }
|   error "define" TOKEN_DATA_TYPE "->" TOKEN_NUMBER "->" TOKEN_DATA_TYPE "{" body "}" error
    {
        printf("Error: Watch your manners!\n");
    }

pipeline:
    TOKEN_NUMBER "->" pipeline
    {
        $$ = ALLOCATE_TYPE(Pipeline);
        $$->type = PIPELINE_TYPE_CALL;
        $$->function = $1;
        $$->next = $3;
    }
|   TOKEN_NUMBER "->" error
    {
        printf("Error: Invalid pipeline entry\n");
    }
|   match_statement "->" pipeline
    {
        $$ = ALLOCATE_TYPE(Pipeline);
        $$->type = PIPELINE_TYPE_MATCH;
        $$->condition_list = $1;
        $$->next = $3;
    }
|   "eval" expression "->" pipeline
    {
        $$ = ALLOCATE_TYPE(Pipeline);
        $$->type = PIPELINE_TYPE_EVAL;
        $$->expression = $2;
        $$->next = $4;
    }
|   TOKEN_NUMBER
    {
        $$ = ALLOCATE_TYPE(Pipeline);
        $$->type = PIPELINE_TYPE_END;
        $$->out = $1;
        $$->next = NULL;
    }

match_statement:
    "match" "{" match_condition_list "}"
    {
        $$ = $3;
    }
|   "match" error
    {
        printf("Error: Invalid match body\n");
    }

match_condition_list:
    TOKEN_COMPARITOR expression ":" "{" body "}" match_condition_list
    {
        $$ = ALLOCATE_TYPE(MatchConditionList);
        $$->comparitor = $1;
        $$->value = $2;
        $$->body = $5;
        $$->next = $7;
    }
|   TOKEN_COMPARITOR expression error "{" body "}" match_condition_list
    {
        printf("Error: Missing ':' before match body\n");
    }
|   expression ":" error
    {
        printf("Error: Missing comparitor on match\n");
    }
|   TOKEN_COMPARITOR ":" error
    {
        printf("Error: Missing comparitor value on match\n");
    }
|   TOKEN_COMPARITOR expression ":" error "{" match_condition_list
    {
        printf("Error: Missing '}' before match body\n");
    }
|   %empty
    {
        $$ = NULL;
    }

expression:
    value
    {
        $$ = ALLOCATE_TYPE(Expression);
        $$->op = OPERATOR_VALUE;
        $$->value = $1;
    }
|   expression TOKEN_OPERATOR expression
    {
        $$ = ALLOCATE_TYPE(Expression);
        $$->lhs = $1;
        $$->op = $2;
        $$->rhs = $3;
    }
|   expression "[" expression "]"
    {
        $$ = ALLOCATE_TYPE(Expression);
        $$->op = OPERATOR_INDEX;
        $$->lhs = $1;
        $$->rhs = $3;
    }
|   "(" expression ")"
    {
        $$ = $2;
    }
|   "[" list "]"
    {
        $$ = ALLOCATE_TYPE(Expression);
        $$->op = OPERATOR_LIST;
        $$->list = $2;
    }

value:
    TOKEN_VALUE
    {
        $$ = $1;
    }
|   TOKEN_NUMBER
    {
        $$.type = VALUE_TYPE_NUMBER;
        $$.num = $1;
    }

list:
    "," expression list
    {
        $$ = ALLOCATE_TYPE(List);
        $$->value = $2;
        $$->next = $3;
    }
|   "," error list
    {
        printf("Error: Expected value after ',' in list\n");
    }
|   error expression list
    {
        printf("Error: Expect ',' at the start of list element\n");
    }
|   %empty
    {
        $$ = NULL;
    }

%%

