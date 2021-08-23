#include "vec.h"

struct Vec vec_new(size_t type_size)
{
    return (struct Vec){ .size = 0, .cont = slab_new(type_size) };
}

void vec_push(struct Vec *vec, void *val)
{
    if (!slab_write(&vec->cont, vec->size, val)) {
        slab_resize(&vec->cont, vec->cont.capacity * 2);
        slab_write(&vec->cont, vec->size, val);
    }
    vec->size += 1;
}

void *vec_get(struct Vec *vec, size_t at)
{
    if (at >= vec->size)
        return NULL;
    return slab_read(&vec->cont, at);
}

void vec_remove(struct Vec *vec, size_t at)
{
    slab_move(&vec->cont, at + 1, at);
    vec->size -= 1;
}

void vec_pop(struct Vec *vec)
{
    if (vec->size > 0)
        vec->size -= 1;
}

void vec_drop(struct Vec *vec)
{
    slab_drop(&vec->cont);
}
