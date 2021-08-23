#pragma once
#ifndef __I__DEPS_VEC_H
#define __I__DEPS_VEC_H

#include <stdbool.h>
#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include "../slab/slab.h"
#define VEC_GET_VAL(T, v, u) *(T*)vec_get(v, u)

struct Vec {
    size_t size;
    struct Slab cont;
};

struct Vec vec_new(size_t etype);
void vec_push(struct Vec *vec, void *val);
void *vec_get(struct Vec *vec, size_t at);
void vec_pop(struct Vec *vec);
void vec_drop(struct Vec *vec);
void vec_remove(struct Vec *vec, size_t at);

#endif
