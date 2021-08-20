#pragma once
#include "../slab/slab.h"
#include "../common/types.h"


struct Arena
{
    struct Slab slab;
    /*
     * Each element in the list will represent if some element is free in the
     * slab 1 = occupied 0 = freed
     */
    u32 *frees;
    usize _free_capacity;
    isize last_index;
};

struct Arena arena_new(usize type_size);
usize arena_first_index(struct Arena *self);
usize arena_advance_index(struct Arena *self, usize index);
usize arena_retreat_index(struct Arena *self, usize index);
int arena_free_index(struct Arena *self, usize index);
usize arena_insert(struct Arena *self, void *value);
bool arena_is_free(struct Arena *self, usize index);
void *arena_get(struct Arena *self, usize index);
void arena_drop(struct Arena *self);
