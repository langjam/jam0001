#ifndef SCOPE_H
#define SCOPE_H

#include "hash_table.h"
#include "object.h"

typedef struct _Scope
{
    HashTable hash_table;
    struct _Scope *parent;
} Scope;

Scope scope_new(Scope *parent);
void scope_free(Scope*);

int scope_add(Scope*, double name, Object *object);
void scope_set(Scope*, double name, Object *object);
Object *scope_get(Scope*, double name);

#endif // SCOPE_H

