#ifndef OBJECT_H
#define OBJECT_H

#include "parser.h"
#include "heap.h"
#include <stdbool.h>

struct _Interpreter;
struct _Scope;
struct _Object;

typedef struct _Object*(*NativeFunction)(struct _Interpreter*, struct _Scope*, Object *argument);

typedef enum _ObjectType
{
    OBJECT_TYPE_NONE,
    OBJECT_TYPE_NUMBER,
    OBJECT_TYPE_STRING,
    OBJECT_TYPE_BOOL,
    OBJECT_TYPE_FUNCTION,
    OBJECT_TYPE_NATIVE_FUNCTION,
    OBJECT_TYPE_LIST,
} ObjectType;

typedef struct _Object
{
    ObjectType type;
    int list_len;
    void *allocator;

    union
    {
        double number;
        int bool_val;
        const char *str;

        Function *function;
        NativeFunction native_function;
        struct _Object *items;
    };
} Object;

Object *object_none(Heap*);
Object *object_number(Heap*, double);
Object *object_string(Heap*, const char *str);
Object *object_bool(Heap*, int);
Object *object_function(Heap*, Function*);
Object *object_native_function(Heap*, NativeFunction);
Object *object_list(Heap*, int len);
void object_free(Object *object);

const char* object_to_string(Object*);
Object *object_index(Heap*, Object *lhs, Object *rhs);
Object *object_add(Heap*, Object *lhs, Object *rhs);
Object *object_subtract(Heap*, Object *lhs, Object *rhs);
Object *object_multiply(Heap*, Object *lhs, Object *rhs);
Object *object_divide(Heap*, Object *lhs, Object *rhs);
bool object_less_than(Object *lhs, Object *rhs);
bool object_more_than(Object *lhs, Object *rhs);
bool object_equals(Object *lhs, Object *rhs);

#endif // OBJECT_H

