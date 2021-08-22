#include "object.h"
#include "heap.h"
#include "interpreter.h"
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <assert.h>

Object *object_none(Heap *heap)
{
    Object *obj = heap_allocate_object(heap);
    obj->type = OBJECT_TYPE_NONE;
    return obj;
}

Object *object_number(Heap *heap, double n)
{
    Object *obj = heap_allocate_object(heap);
    obj->type = OBJECT_TYPE_NUMBER;
    obj->number = n;
    return obj;
}

Object *object_string(Heap *heap, const char *str)
{
    char *buffer = malloc(strlen(str) + 1);
    strcpy(buffer, str);

    Object *obj = heap_allocate_object(heap);
    obj->type = OBJECT_TYPE_STRING;
    obj->str = buffer;
    return obj;
}

Object *object_bool(Heap *heap, int b)
{
    Object *obj = heap_allocate_object(heap);
    obj->type = OBJECT_TYPE_BOOL;
    obj->bool_val = b;
    return obj;
}

Object *object_function(Heap *heap, Function *function)
{
    Object *obj = heap_allocate_object(heap);
    obj->type = OBJECT_TYPE_FUNCTION;
    obj->function = function;
    return obj;
}

Object *object_native_function(Heap *heap, NativeFunction native_function)
{
    Object *obj = heap_allocate_object(heap);
    obj->type = OBJECT_TYPE_NATIVE_FUNCTION;
    obj->native_function = native_function;
    return obj;
}

Object *object_list(Heap *heap, int len)
{
    Object *obj = heap_allocate_object(heap);
    obj->list_len = len;
    obj->type = OBJECT_TYPE_LIST;
    obj->items = heap_allocate_list(heap, len);
    return obj;
}

void object_free(Object *object)
{
    switch (object->type)
    {
        case OBJECT_TYPE_STRING:
            free((void*)object->str);
            break;

        defualt:
            break;
    }
}

Object *object_index(Heap *heap, Object *lhs, Object *rhs)
{
    if ((lhs->type != OBJECT_TYPE_LIST && lhs->type != OBJECT_TYPE_STRING) ||
        rhs->type != OBJECT_TYPE_NUMBER)
    {
        return object_none(heap);
    }

    switch (lhs->type)
    {
        case OBJECT_TYPE_LIST:
        {
            int start_index = lhs->list_len / 2;
            int index = start_index + (int)rhs->number;
            if (index < 0 || index >= lhs->list_len)
                return object_none(heap);
            else
                return &lhs->items[index];
        }

        case OBJECT_TYPE_STRING:
        {
            int len = strlen(lhs->str);
            int start_index = len / 2;
            int index = start_index + (int)rhs->number;
            if (index < 0 || index > len)
                return object_none(heap);
            else
                return object_number(heap, lhs->str[index]);
        }

        default:
            break;
    }

    assert(0);
    return NULL;
}

Object *object_add(Heap *heap, Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return object_number(heap, lhs->number + rhs->number);

                case OBJECT_TYPE_LIST:
                {
                    Object *list = object_list(heap, 1 + rhs->list_len);
                    memcpy(&list->items[0], lhs, sizeof(Object));
                    for (int i = 0; i < rhs->list_len; i++)
                        list->items[i + 1] = rhs->items[i];
                    return list;
                }

                case OBJECT_TYPE_STRING:
                {
                    size_t size = snprintf(NULL, 0, "%.6g%s", lhs->number, rhs->str);
                    char *str = malloc(size + 1);
                    sprintf(str, "%.6g%s", lhs->number, rhs->str);
                    return object_string(heap, str);
                }

                default:
                    return object_none(heap);
            }

        case OBJECT_TYPE_LIST:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                {
                    Object *list = object_list(heap, 1 + lhs->list_len);
                    for (int i = 0; i < lhs->list_len; i++)
                        list->items[i] = lhs->items[i];
                    memcpy(&list->items[lhs->list_len], rhs, sizeof(Object));
                    return list;
                }

                case OBJECT_TYPE_LIST:
                {
                    Object *list = object_list(heap, lhs->list_len + rhs->list_len);
                    for (int i = 0; i < lhs->list_len; i++)
                        list->items[i] = lhs->items[i];
                    for (int i = 0; i < rhs->list_len; i++)
                        list->items[i + lhs->list_len] = rhs->items[i];
                    return list;
                }

                default:
                    return object_none(heap);
            }

        case OBJECT_TYPE_STRING:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                {
                    size_t size = snprintf(NULL, 0, "%s%.6g", lhs->str, rhs->number);
                    char *str = malloc(size + 1);
                    sprintf(str, "%s%.6g", lhs->str, rhs->number);
                    return object_string(heap, str);
                }
                
                case OBJECT_TYPE_STRING:
                {
                    size_t lhs_size = strlen(lhs->str);
                    size_t rhs_size = strlen(rhs->str);
                    char *str = malloc(lhs_size + rhs_size + 1);
                    memcpy(str, lhs->str, lhs_size);
                    memcpy(str + lhs_size, rhs->str, rhs_size);
                    str[lhs_size + rhs_size] = '\0';
                    return object_string(heap, str);
                }

                default:
                    object_none(heap);
            }

        default:
            return object_none(heap);
    }
}

Object *object_subtract(Heap *heap, Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return object_number(heap, lhs->number - rhs->number);

                default:
                    return object_none(heap);
            }

        default:
            return object_none(heap);
    }
}

Object *object_multiply(Heap *heap, Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return object_number(heap, lhs->number * rhs->number);

                defualt:
                    return object_none(heap);
            }

        default:
            return object_none(heap);
    }
}

Object *object_divide(Heap *heap, Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return object_number(heap, lhs->number / rhs->number);

                defualt:
                    return object_none(heap);
            }

        default:
            return object_none(heap);
    }
}

bool object_less_than(Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return lhs->number < rhs->number;

                default:
                    return false;
            }

        default:
            return false;
    }
}

bool object_more_than(Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return lhs->number > rhs->number;

                default:
                    return false;
            }

        default:
            return false;
    }
}

bool object_equals(Object *lhs, Object *rhs)
{
    switch (lhs->type)
    {
        case OBJECT_TYPE_NUMBER:
            switch (rhs->type)
            {
                case OBJECT_TYPE_NUMBER:
                    return lhs->number == rhs->number;

                default:
                    return false;
            }

        default:
            return false;
    }
}

// FIXME: Buffer overflow
const char* object_to_string(Object *object)
{
    static char object_string_buffer[80];

    switch (object->type)
    {
        case OBJECT_TYPE_NONE:
            sprintf(object_string_buffer, "None");
            break;

        case OBJECT_TYPE_NUMBER:
            sprintf(object_string_buffer, "%.6g", object->number);
            break;

        case OBJECT_TYPE_STRING:
            sprintf(object_string_buffer, "%s", object->str);
            break;

        case OBJECT_TYPE_BOOL:
            sprintf(object_string_buffer, "%s", object->bool_val ? "true" : "false");
            break;

        case OBJECT_TYPE_FUNCTION:
            sprintf(object_string_buffer, "Function '%.6g'", object->function->name);
            break;

        case OBJECT_TYPE_NATIVE_FUNCTION:
            sprintf(object_string_buffer, "<Native Function 0x%lx>", (unsigned long)object->native_function);
            break;

        case OBJECT_TYPE_LIST:
        {
            char buffer[80];
            sprintf(buffer, "[");
            for (int i = 0; i < object->list_len; i++)
            {
                if (i != 0)
                    sprintf(buffer + strlen(buffer), " ");
                sprintf(buffer + strlen(buffer), "%s", object_to_string(&object->items[i]));
            }
            sprintf(buffer + strlen(buffer), "]");
            memcpy(object_string_buffer, buffer, 80);
            break;
        }
    }

    return object_string_buffer;
}

