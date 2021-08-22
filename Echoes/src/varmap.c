#include "varmap.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


static size_t map_hash(struct VariableMap *map, char* const to_hash) {
    size_t value = 0;
    char *cur_idx;

    for (cur_idx = to_hash; *cur_idx; ++cur_idx) {
        value *= 256;
        value += *cur_idx;
        value %= map->allocated;
    }

    return value;
}

void map_construct(struct VariableMap *map) {
    map->buckets = NULL;
    map->allocated = 0;
    map->pairs = 0;
}

static void node_delete(struct BucketNode *node) {
    if (!node)
        return;
    free(node->key);
    node_delete(node->next);
    free(node);
}

void map_dealloc(struct VariableMap *map) {
    size_t i;
    for (i = 0; i < map->allocated; ++i)
        node_delete(map->buckets[i]);
     free(map);
}

bool map_set(struct VariableMap *map, char *key, struct Value *value) {
    struct BucketNode *node;
    struct BucketNode *previous_node;
    size_t hash_result;

    if (map->allocated == 0) {
        map->buckets = calloc(8, sizeof(struct BucketNode *));
        if (!map->buckets)
            return false;
        map->allocated = 8;
    }

    hash_result = map_hash(map, key);
    previous_node = NULL;
    for (node = map->buckets[hash_result]; node; node = node->next) {
        if (!node->key || strcmp(node->key, key) == 0) {
            /* deallocate value if already exists at key */
            // value_dealloc(&node->value);
            node->key = key;
            node->value = value;
            return true;
        }
        previous_node = node;

    }
    if (!(node = malloc(sizeof(struct BucketNode))))
        return false;

    node->key = key;
    node->value = value;

    if (!previous_node)
        map->buckets[hash_result] = node;
    else
        previous_node->next = node;

    ++map->pairs;
    return true;
}

struct Value *map_get(struct VariableMap *map, char* key) {
    struct BucketNode *node;

    if (map->allocated == 0) /* not yet allocated (map_set wasn't yet called) */
        return NULL;
    for (node = map->buckets[map_hash(map, key)]; node; node = node->next) {
        if (strcmp(key, node->key) == 0)
            return node->value;
    }

    return NULL;
}