#include "interpreter.h"
#include "hash_table.h"
#include "heap.h"
#include "object.h"
#include "parser.h"
#include "scope.h"
#include "base64.h"
#include "standard_library.h"
#include <assert.h>
#include <stdlib.h>
#include <memory.h>
#include <stdbool.h>
#include <math.h>

static Object *eval_body(Interpreter*, Scope*, Body*, Object *argument);
static Object *eval_function(Interpreter*, Scope*, Function*, Object *argument);
static Object *eval_expression(Interpreter*, Scope*, Expression*);
static void eval_statement(Interpreter*, Scope*, Statement*);

static void garbage_collect(Interpreter *interpreter, Scope *scope)
{
    for (Scope *local = scope; local; local = local->parent)
    {
        for (HashTableItorator it = hash_table_itorator(&local->hash_table); !isnan(it.key); hash_table_next(&it))
            heap_visit((Object*)it.value);
    }
    heap_do_garbage_collection(interpreter->heap);
}

static Interpreter new_interpreter(Body *program)
{
    Interpreter interpreter;
    interpreter.heap = heap_new();
    interpreter.global_scope = scope_new(NULL);
    standard_library_init(interpreter.heap, &interpreter.global_scope);
    return interpreter;
}

static void free_interpreter(Interpreter *interpreter)
{
    heap_free(interpreter->heap);
    scope_free(&interpreter->global_scope);
    free(interpreter->heap);
}

static Object *eval_value(Interpreter *interpreter, Scope *scope,
                          Value *value)
{
    switch (value->type)
    {
        case VALUE_TYPE_NONE:
            return object_none(interpreter->heap);

        case VALUE_TYPE_NUMBER:
        {
            Object *override = scope_get(scope, value->num);
            if (override)
                return override;
            else
                return object_number(interpreter->heap, value->num);
        }

        case VALUE_TYPE_BOOL:
            return object_bool(interpreter->heap, value->bool_val);

        case VALUE_TYPE_STRING:
            return object_string(interpreter->heap, value->str);
    }
}

static Object *eval_list(Interpreter *interpreter, Scope *scope,
                         List *list)
{
    int len = 0;
    for (List *it = list; it; it = it->next)
        len += 1;

    int index = 0;
    Object *list_object = object_list(interpreter->heap, len);
    for (List *it = list; it; it = it->next)
        memcpy(&list_object->items[index++], eval_expression(interpreter, scope, it->value), sizeof(Object));

    return list_object;
}

static Object *eval_expression(Interpreter *interpreter, Scope *scope,
                               Expression *expression)
{
    if (!expression)
        return object_none(interpreter->heap);

    switch (expression->op)
    {
        case OPERATOR_VALUE:
            return eval_value(interpreter, scope, &expression->value);

        case OPERATOR_LIST:
            return eval_list(interpreter, scope, expression->list);

        case OPERATOR_INDEX:
        case OPERATOR_ADD:
        case OPERATOR_SUBTRACT:
        case OPERATOR_MULTIPLY:
        case OPERATOR_DIVIDE:
        {
            Object *lhs = eval_expression(interpreter, scope, expression->lhs);
            Object *rhs = eval_expression(interpreter, scope, expression->rhs);
            switch (expression->op)
            {
                case OPERATOR_INDEX:
                    return object_index(interpreter->heap, lhs, rhs);
                case OPERATOR_ADD:
                    return object_add(interpreter->heap, lhs, rhs);
                case OPERATOR_SUBTRACT:
                    return object_subtract(interpreter->heap, lhs, rhs);
                case OPERATOR_MULTIPLY:
                    return object_multiply(interpreter->heap, lhs, rhs);
                case OPERATOR_DIVIDE:
                    return object_divide(interpreter->heap, lhs, rhs);
            }
        }
    }

    assert(0);
    return NULL;
}

static bool is_object_data_type(Object *object, DataType data_type)
{
    switch (data_type)
    {
        case DATA_TYPE_NONE:
            return object->type == OBJECT_TYPE_NONE;

        case DATA_TYPE_NUMBER:
            return object->type == OBJECT_TYPE_NUMBER;

        case DATA_TYPE_STRING:
            return object->type == OBJECT_TYPE_STRING;

        case DATA_TYPE_LIST:
            return object->type == OBJECT_TYPE_LIST;
    }
}

static const char *data_type_to_string(DataType data_type)
{
    switch (data_type)
    {
        case DATA_TYPE_NONE: return "none";
        case DATA_TYPE_NUMBER: return "number";
        case DATA_TYPE_STRING: return "string";
        case DATA_TYPE_LIST: return "list";
    }
}

static Object *eval_call(Interpreter *interpreter, Scope *scope,
                         double function, Object *argument)
{
    Object *function_object = scope_get(scope, function);
    if (!function_object)
    {
        printf("Error: No symbol with the name '%.6g' found\n", function);
        return object_none(interpreter->heap);
    }

    switch (function_object->type)
    {
        case OBJECT_TYPE_FUNCTION:
        {
            Function *function = function_object->function;
            if (is_object_data_type(argument, function->parameter_type))
            {
                printf("Error: Argument to '%.6g' cannot be '%s'\n", 
                    function->name, 
                    data_type_to_string(function->parameter_type));
                return object_none(interpreter->heap);
            }

            return eval_function(interpreter, scope, function, argument);
        }

        case OBJECT_TYPE_NATIVE_FUNCTION:
            return function_object->native_function(interpreter, scope, argument);

        default:
            printf("Error: '%.6g' not a function\n", function);
            return object_none(interpreter->heap);
    }

    assert(0);
    return NULL;
}

static bool doas_condition_match(Interpreter *interpreter, Scope *scope,
                                 Object *lhs, MatchConditionList *condition)
{
    Object *rhs = eval_expression(interpreter, scope, condition->value);
    switch (condition->comparitor)
    {
        case COMPARITOR_LESS_THAN_OR_EQUAL:
            return !object_more_than(lhs, rhs);
        case COMPARITOR_MORE_THAN_OR_EQUAL:
            return !object_less_than(lhs, rhs);
        case COMPARITOR_LESS_THAN:
            return object_less_than(lhs, rhs);
        case COMPARITOR_MORE_THAN:
            return object_more_than(lhs, rhs);
        case COMPARITOR_EQUALS:
            return object_equals(lhs, rhs);
    }
}

static Object *eval_match(Interpreter *interpreter, Scope *scope,
                          MatchConditionList *match, Object *argument)
{
    Object *return_value = NULL;
    for (MatchConditionList *it = match; it; it = it->next)
    {
        if (!doas_condition_match(interpreter, scope, argument, it))
        {
            return_value = eval_body(interpreter, scope, it->body, argument);
            break;
        }
    }

    if (!return_value)
        return object_none(interpreter->heap);
    else
        return return_value;
}

static Object *eval_eval(Interpreter *interpreter, Scope *scope,
                      Expression *expression, Object *argument)
{
    Scope local_scope = scope_new(scope);
    scope_set(&local_scope, base64_decode_double("in"), argument);
    Object *result = eval_expression(interpreter, &local_scope, expression);

    scope_free(&local_scope);
    return result;
}

static Object *eval_function(Interpreter *interpreter, Scope *scope, 
                             Function *function, Object *argument)
{
    if (!function)
        return object_none(interpreter->heap);

    Object *return_value = eval_body(interpreter, scope, function->body, argument);
    garbage_collect(interpreter, scope);

    if (return_value)
        return return_value;
    else
        return object_none(interpreter->heap);
}

static void eval_statement(Interpreter *interpreter, Scope *scope,
                           Statement *statement)
{
    if (!statement)
        return;

    Object *argument = eval_expression(interpreter, scope, statement->in);
    for (Pipeline *it = statement->pipeline; it; it = it->next)
    {
        switch (it->type)
        {
            case PIPELINE_TYPE_CALL:
                argument = eval_call(interpreter, scope, it->function, argument);
                break;

            case PIPELINE_TYPE_MATCH:
                argument = eval_match(interpreter, scope, it->condition_list, argument);
                break;

            case PIPELINE_TYPE_EVAL:
                argument = eval_eval(interpreter, scope, it->expression, argument);
                break;

            case PIPELINE_TYPE_END:
                if (!isnan(it->out))
                    scope_set(scope, it->out, argument);
                break;
        }
    }
}

static void build_scope(Heap *heap, Scope *scope, Body *body)
{
    for (Body *it = body; it; it = it->next)
    {
        if (!it->statement ||
            it->statement->type != STATEMENT_TYPE_FUNCTION ||
            !it->statement->function)
        {
            continue;
        }

        Function *function = it->statement->function;
        scope_add(scope, function->name, object_function(heap, function));
    }
}

static Object *eval_body(Interpreter *interpreter, Scope *scope, 
                         Body *body, Object *argument)
{
    Scope local_scope = scope_new(scope);
    build_scope(interpreter->heap, &local_scope, body);

    if (argument)
        scope_set(&local_scope, base64_decode_double("in"), argument);

    for (Body *it = body; it; it = it->next)
        eval_statement(interpreter, &local_scope, it->statement);

    Object *return_value = scope_get(&local_scope, base64_decode_double("out"));
    scope_free(&local_scope);

    return return_value;
}

void interpreter_run(Body *program)
{
    Interpreter interpreter = new_interpreter(program);
    eval_body(&interpreter, &interpreter.global_scope, program, NULL);
    free_interpreter(&interpreter);
}

