#include "scope.h"
#include "hash_table.h"
#include "object.h"
#include <math.h>
#include <assert.h>

Scope scope_new(Scope *parent)
{
    Scope scope;
    scope.hash_table = hash_table_new();
    scope.parent = parent;
    return scope;
}

void scope_free(Scope *scope)
{
    hash_table_free(&scope->hash_table);
}

int scope_add(Scope *scope, double name, Object *object)
{
    assert(!isnan(name));
    return hash_table_add(&scope->hash_table, name, (void*)object);
}

void scope_set(Scope *scope, double name, Object *object)
{
    assert(!isnan(name));
    return hash_table_set(&scope->hash_table, name, object);
}

Object *scope_get(Scope *scope, double name)
{
    Object *object = (Object*)hash_table_get(&scope->hash_table, name);
    if (object)
        return object;

    if (scope->parent)
        return scope_get(scope->parent, name);

    return NULL;
}

