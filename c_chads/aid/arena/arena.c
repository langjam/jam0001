#include "arena.h"
#include <math.h>
#include <errno.h>
#include <stdio.h>

#define FREED_BITS 32UL

struct Arena arena_new(usize type_size)
{
    const usize alloc_cnt =
    #ifdef NDEBUG
    1;
    #else
    32;
    #endif
    return (struct Arena) { 
        .slab = slab_new(type_size),
        .last_index = -1,
        ._free_capacity = alloc_cnt,
        .frees = calloc(alloc_cnt, sizeof(u32))
    };
}

// Returns rightmost set bit
u32 rmsb(u32 num)
{
    return (u32)log2((double)(num & ~(num-1)));
}

usize arena_first_index(struct Arena *self)
{
    usize index = 0;
    while (arena_is_free(self, index) && (isize)index < self->last_index)
        index += 1;
    return index;
}

usize arena_advance_index(struct Arena *self, usize index)
{
    index += 1;
    while (arena_is_free(self, index) && (isize)index < self->last_index)
        index += 1;
    return index;
}

usize arena_retreat_index(struct Arena *self, usize index)
{
    if (index == 0)
        return 0;
    index -= 1;
    while (arena_is_free(self, index) && index > 0)
        index -= 1;
    return index;
}

usize arena_insert(struct Arena *self, void *value)
{
    for (usize i = 0; i < self->_free_capacity; i += 1) {
        u32 bitmap = self->frees[i];
        if (bitmap != 0xFFFFFFFF) {
            u32 unset_pos = (u32)rmsb(~bitmap);
            #ifdef NDEBUG
            if (unset_pos >= 32) {
                fprintf(stderr, "Unset position exceeded 32 (internal error)");
                exit(-1);
            }
            #endif
            usize offset = (i * FREED_BITS) + unset_pos;
            if (self->slab.capacity <= offset) {
                if (!slab_resize(&self->slab, 
                #ifdef NDEBUG
                    self->slab.capacity + 1
                #else
                    self->slab.capacity * 2
                #endif
                )) {
                    fprintf(stderr, "YOUR CONTAINER IS TOO GODDAMN BIG!\n");
                    exit(-1);
                }
            }
            if ((isize)offset > self->last_index) {
                self->last_index = (isize)offset;
            }
            slab_write(&self->slab, offset, value);
            self->frees[i] |= ((u32)1 << unset_pos);
            return offset;
        }
    }

    if (self->_free_capacity == 0) self->_free_capacity = 1; // Make clang shutuppp

    // Resize and repeat
    void *old_frees = self->frees;
    void *new_frees = calloc(self->_free_capacity * 2, sizeof(u32));
    memcpy(new_frees, self->frees, self->_free_capacity * sizeof(u32));
    free(old_frees);
    self->frees = new_frees;
    self->_free_capacity *= 2;


    return arena_insert(self, value);
}

void *arena_get(struct Arena *self, usize index)
{
    usize bitmap_offset = index / FREED_BITS;
    u32 flag = 1 << (u32)(index - bitmap_offset * FREED_BITS);

    if (bitmap_offset > self->_free_capacity) {
        #ifdef NDEBUG
        fprintf(
            stderr,
            "Arena container: invalid bitmap offset, maximum %zu got %zu, "
            "requested via index %zu\n",
            self->_free_capacity,
            bitmap_offset,
            index);
        #endif
        return NULL;
    }
    if ((self->frees[bitmap_offset] & flag) == 0) {
        #ifdef NDEBUG
        fprintf(stderr, "Arena container: attempt to access freed area\n");
        #endif
        return NULL;
    }

    return slab_read(&self->slab, index);
}

bool arena_is_free(struct Arena *self, usize index)
{
    usize bitmap_offset = index / FREED_BITS;
    u32 flag = (u32)1 << (u32)(index - bitmap_offset * FREED_BITS);
    if (bitmap_offset >= self->_free_capacity)
        return true;
    if ((self->frees[bitmap_offset] & flag) == 0) {
        return true;
    }

    return false;
}

int arena_free_index(struct Arena *self, usize index)
{
    usize bitmap_offset = index / FREED_BITS;
    u32 flag = 1 << (index - bitmap_offset * FREED_BITS);

    if (bitmap_offset > self->_free_capacity) {
        #ifdef NDEBUG
        fprintf(
            stderr,
            "Arena container: invalid bitmap offset, maximum %zu got %zu, "
            "requested via index %zu",
            self->_free_capacity,
            bitmap_offset,
            index);
        #endif
        return -ERANGE;
    }
    if ((self->frees[bitmap_offset] & flag) == 0) {
        #ifdef NDEBUG
        fprintf(stderr, "Arena container: attempt to free already freed area");
        #endif
        return -ENODATA;
    }

    // Mark free
    self->frees[bitmap_offset] &= ~flag;
    return 0;
}

void arena_drop(struct Arena *self)
{
    slab_drop(&self->slab);
    free(self->frees);
}

