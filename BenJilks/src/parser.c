#include "parser.h"
#include <assert.h>

static void debug_log_body(Body*, int indent);

static void print_indent(int indent)
{
    for (int i = 0; i < indent; i++)
        printf("    ");
}

static const char *datatype_to_string(DataType data_type)
{
    switch (data_type)
    {
        case DATA_TYPE_NONE: return "none";
        case DATA_TYPE_NUMBER: return "number";
        case DATA_TYPE_STRING: return "string";
        case DATA_TYPE_LIST: return "list";
    }
}

static void debug_log_value(Value *value, int indent)
{
    switch (value->type)
    {
        case VALUE_TYPE_NONE:
            print_indent(indent);
            printf("None\n");
            break;

        case VALUE_TYPE_NUMBER:
            print_indent(indent);
            printf("Number=%.6g\n", value->num);
            break;

        case VALUE_TYPE_STRING:
            print_indent(indent);
            printf("String='%s'\n", value->str);
            break;

        default:
            assert(0);
    }
}

static void debug_log_expression(Expression *expression, int indent)
{
    if (!expression)
        return;

    switch (expression->op)
    {
        case OPERATOR_VALUE:
            print_indent(indent);
            printf("Value:\n");
            debug_log_value(&expression->value, indent + 1);
            break;

        case OPERATOR_LIST:
            print_indent(indent);
            printf("List:\n");
            for (List *it = expression->list; it; it = it->next)
                debug_log_expression(it->value, indent + 1);
            break;

        case OPERATOR_INDEX:
        case OPERATOR_ADD:
        case OPERATOR_SUBTRACT:
        case OPERATOR_MULTIPLY:
        case OPERATOR_DIVIDE:
            print_indent(indent);
            switch (expression->op)
            {
                case OPERATOR_INDEX: printf("[]:\n"); break;
                case OPERATOR_ADD: printf("+:\n"); break;
                case OPERATOR_SUBTRACT: printf("-:\n"); break;
                case OPERATOR_MULTIPLY: printf("*:\n"); break;
                case OPERATOR_DIVIDE: printf("/:\n"); break;
            }

            debug_log_expression(expression->lhs, indent + 1);
            debug_log_expression(expression->rhs, indent + 1);
            break;

        default:
            assert(0);
    }
}

static void debug_log_match_condition_list(MatchConditionList *list, int indent)
{
    for (MatchConditionList *it = list; it; it = it->next)
    {
        print_indent(indent);
        switch (it->comparitor)
        {
            case COMPARITOR_LESS_THAN_OR_EQUAL: printf("<=\n"); break;
            case COMPARITOR_MORE_THAN_OR_EQUAL: printf(">=\n"); break;
            case COMPARITOR_LESS_THAN: printf("<\n"); break;
            case COMPARITOR_MORE_THAN: printf(">\n"); break;
            case COMPARITOR_EQUALS: printf("=\n"); break;
        }
        debug_log_expression(it->value, indent + 1);

        print_indent(indent);
        printf("Body:\n");
        debug_log_body(it->body, indent + 1);
    }
}

static void debug_log_pipeline(Pipeline *pipeline, int indent)
{
    for (Pipeline *it = pipeline; it; it = it->next)
    {
        switch (it->type)
        {
            case PIPELINE_TYPE_CALL:
                print_indent(indent);
                printf("Function: %.6g\n", it->function);
                break;

            case PIPELINE_TYPE_MATCH:
                print_indent(indent);
                printf("Match:\n");
                debug_log_match_condition_list(it->condition_list, indent + 1);
                break;

            case PIPELINE_TYPE_EVAL:
                print_indent(indent);
                printf("Eval:\n");
                debug_log_expression(it->expression, indent + 1);
                break;

            case PIPELINE_TYPE_END:
                print_indent(indent);
                printf("Out: %.6g\n", it->out);
                break;
        }
    }
}

static void debug_log_function(Function *function, int indent)
{
    if (!function)
        return;

    print_indent(indent);
    printf("Function !%s -> %.6g -> !%s\n", 
        datatype_to_string(function->parameter_type),
        function->name,
        datatype_to_string(function->return_type));

    debug_log_body(function->body, indent + 1);
}

static void debug_log_statement(Statement *statement, int indent)
{
    if (!statement)
        return;

    print_indent(indent);
    printf("Statement:\n");
    switch (statement->type)
    {
        case STATEMENT_TYPE_FUNCTION:
            debug_log_function(statement->function, indent + 1);
            break;

        case STATEMENT_TYPE_PIPELINE:
            print_indent(indent + 1);
            printf("In:\n");
            debug_log_expression(statement->in, indent + 2);

            print_indent(indent + 1);
            printf("Pipeline:\n");
            debug_log_pipeline(statement->pipeline, indent + 2);
            break;
    }
}

static void debug_log_body(Body *body, int indent)
{
    for (Body *it = body; it; it = it->next)
        debug_log_statement(it->statement, indent);
}

void debug_log_program(Body *program)
{
    debug_log_body(program, 0);
}

