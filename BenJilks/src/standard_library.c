#include "standard_library.h"
#include "base64.h"
#include "heap.h"
#include "object.h"
#include "scope.h"
#include "interpreter.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

Object *cat(Interpreter *interpreter, Scope *scope, Object *argument)
{
    return argument;
}

Object *tee(Interpreter *interpreter, Scope *scope, Object *argument)
{
    if (argument->type != OBJECT_TYPE_LIST ||
        argument->list_len != 2 ||
        argument->items[1].type != OBJECT_TYPE_STRING)
    {
        return object_none(interpreter->heap);
    }

    scope_set(scope, base64_decode_double(argument->items[1].str), &argument->items[0]);
    return &argument->items[0];
}

Object *print(Interpreter *interpreter, Scope *scope, Object *argument)
{
    printf("%s\n", object_to_string(argument));
    return object_none(interpreter->heap);
}

Object *len(Interpreter *interpreter, Scope *scope, Object *argument)
{
    switch (argument->type)
    {
        case OBJECT_TYPE_NONE:
        case OBJECT_TYPE_NUMBER:
        case OBJECT_TYPE_BOOL:
        case OBJECT_TYPE_FUNCTION:
        case OBJECT_TYPE_NATIVE_FUNCTION:
            return object_number(interpreter->heap, 1);

        case OBJECT_TYPE_STRING:
            return object_number(interpreter->heap, strlen(argument->str));

        case OBJECT_TYPE_LIST:
            return object_number(interpreter->heap, argument->list_len);
    }
}

Object *right(Interpreter *interpreter, Scope *scope, Object *argument)
{
    if (argument->type != OBJECT_TYPE_LIST ||
        argument->list_len != 2 ||
        (argument->items[0].type != OBJECT_TYPE_LIST && argument[0].type != OBJECT_TYPE_STRING) ||
        argument->items[1].type != OBJECT_TYPE_NUMBER)
    {
        return object_none(interpreter->heap);
    }

    Object *arr = &argument->items[0];
    Object *right_of = &argument->items[1];
    switch (argument->items[0].type)
    {
        case OBJECT_TYPE_LIST:
        {
            int index = arr->list_len / 2 + (int)right_of->number;
            if (index < 0 || index >= arr->list_len)
                return object_none(interpreter->heap);

            Object *result = object_list(interpreter->heap, arr->list_len - index - 1);
            for (int i = 0; i < arr->list_len - index - 1; i++)
                memcpy(&result->items[i], &arr->items[i + index + 1], sizeof(Object));
            return result;
        }

        case OBJECT_TYPE_STRING:
            break;

        default:
            break;
    }

    assert(0);
    return NULL;
}

Object *left(Interpreter *interpreter, Scope *scope, Object *argument)
{
    if (argument->type != OBJECT_TYPE_LIST ||
        argument->list_len != 2 ||
        (argument->items[0].type != OBJECT_TYPE_LIST && argument[0].type != OBJECT_TYPE_STRING) ||
        argument->items[1].type != OBJECT_TYPE_NUMBER)
    {
        return object_none(interpreter->heap);
    }

    Object *arr = &argument->items[0];
    Object *right_of = &argument->items[1];
    switch (argument->items[0].type)
    {
        case OBJECT_TYPE_LIST:
        {
            int index = arr->list_len / 2 + (int)right_of->number;
            if (index < 0 || index >= arr->list_len)
                return object_none(interpreter->heap);

            Object *result = object_list(interpreter->heap, arr->list_len - index - 1);
            for (int i = 0; i < index; i++)
                memcpy(&result->items[i], &arr->items[i], sizeof(Object));
            return result;
        }

        case OBJECT_TYPE_STRING:
            break;

        default:
            break;
    }

    assert(0);
    return NULL;
}

Object *_remove(Interpreter *interpreter, Scope *scope, Object *argument)
{
    if (argument->type != OBJECT_TYPE_LIST ||
        argument->list_len != 2 ||
        (argument->items[0].type != OBJECT_TYPE_LIST && argument[0].type != OBJECT_TYPE_STRING) ||
        argument->items[1].type != OBJECT_TYPE_NUMBER)
    {
        return object_none(interpreter->heap);
    }

    Object *arr = &argument->items[0];
    Object *right_of = &argument->items[1];
    switch (argument->items[0].type)
    {
        case OBJECT_TYPE_LIST:
        {
            int index = arr->list_len / 2 + (int)right_of->number;
            if (index < 0 || index >= arr->list_len)
                return object_none(interpreter->heap);

            Object *result = object_list(interpreter->heap, arr->list_len - 1);
            int new_index = 0;
            for (int i = 0; i < arr->list_len; i++)
            {
                if (i != index)
                    memcpy(&result->items[new_index++], &arr->items[i], sizeof(Object));
            }
            return result;
        }

        case OBJECT_TYPE_STRING:
            break;

        default:
            break;
    }

    assert(0);
    return NULL;
}

Object *_random(Interpreter *interpreter, Scope *scope, Object *argument)
{
    return object_number(interpreter->heap, (double)rand());
}

Object *input(Interpreter *interpreter, Scope *scope, Object *argument)
{
    printf("%s", object_to_string(argument));

    char *line = NULL;
    size_t line_len = 0;
    getline(&line, &line_len, stdin);

    return object_string(interpreter->heap, line);
}

Object *to_int(Interpreter *interpreter, Scope *scope, Object *argument)
{
    switch (argument->type)
    {
        case OBJECT_TYPE_NONE:
        case OBJECT_TYPE_NUMBER:
            return argument;

        case OBJECT_TYPE_FUNCTION:
        case OBJECT_TYPE_NATIVE_FUNCTION:
        case OBJECT_TYPE_LIST:
            return object_none(interpreter->heap);

        case OBJECT_TYPE_STRING:
            return object_number(interpreter->heap, atof(argument->str));
    }
}

Object *_sleep(Interpreter *interpreter, Scope *scope, Object *argument)
{
    if (argument->type == OBJECT_TYPE_NUMBER)
        sleep(argument->number);
    return object_none(interpreter->heap);
}

void standard_library_init(Heap *heap, Scope *scope)
{
    scope_add(scope, base64_decode_double("cat"), object_native_function(heap, cat));
    scope_add(scope, base64_decode_double("tee"), object_native_function(heap, tee));
    scope_add(scope, base64_decode_double("print"), object_native_function(heap, print));
    scope_add(scope, base64_decode_double("len"), object_native_function(heap, len));
    scope_add(scope, base64_decode_double("right"), object_native_function(heap, right));
    scope_add(scope, base64_decode_double("left"), object_native_function(heap, left));
    scope_add(scope, base64_decode_double("remove"), object_native_function(heap, _remove));
    scope_add(scope, base64_decode_double("rand"), object_native_function(heap, _random));
    scope_add(scope, base64_decode_double("input"), object_native_function(heap, input));
    scope_add(scope, base64_decode_double("to_int"), object_native_function(heap, to_int));
    scope_add(scope, base64_decode_double("sleep"), object_native_function(heap, _sleep));
}

