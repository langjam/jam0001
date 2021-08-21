// This uses vector
#include <stdio.h>
#include "map.h"

struct Map map_new(usize el_size) {
    return (struct Map) {
        .keys = vec_new(sizeof(strview_t)),
        .values = vec_new(el_size)
    };
}

void map_insert(struct Map *self, strview_t key, void *value) {
    vec_push(&self->keys, &key);
    vec_push(&self->values, value);
}

void* map_get(struct Map *self, strview_t key) {
    usize index;
    bool found = false;
    for (index = 0; index < self->keys.size; index += 1) {
        strview_t *k = vec_get(&self->keys, index);
        if (strview_eq(key, *k)) {
            found = true;
            break;
        }
    }
    if (!found) return NULL;
    return vec_get(&self->values, index);
}

void map_remove(struct Map *self, strview_t key) {
    usize index;
    bool found = false;
    for (index = 0; index < self->keys.size; index += 1) {
        strview_t *k = vec_get(&self->keys, index);
        if (strview_eq(key, *k)) {
            found = true;
            break;
        }
    }
    if (!found) {
        fprintf(stderr, "Map: Invalid free, key `%.*s`, doesn't exist", (int)key.size, key.view);
        exit(1);
    }
    vec_remove(&self->keys, index);
    vec_remove(&self->values, index);
}

void map_drop(struct Map *self) {
    vec_drop(&self->keys);
    vec_drop(&self->values);
}
