#ifndef ECHOES_TYPES_H
#define ECHOES_TYPES_H

#include "parser.h"

#include <stddef.h>
# include <stdbool.h>

struct VariableMap {
    struct BucketNode {
        struct BucketNode *next;
        char *key;
        struct Value *value;
    } **buckets;
    size_t allocated, pairs;
};

void map_construct(struct VariableMap *map);

bool map_set(struct VariableMap *map, char *key, struct Value *value);

void map_dealloc(struct VariableMap *map);

struct Value *map_get(struct VariableMap *map, char *key);

#endif /* ECHOES_TYPES_H */
