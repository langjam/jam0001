#pragma once
#ifndef __I__DEPS_SLAB_H
#define __I__DEPS_SLAB_H

#include <stdbool.h>
#include <memory.h>
#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include "../common/types.h"

#define OFTYPE(t) sizeof(t)

struct Slab {
    usize el_size;
    usize capacity;
    u8 *elements;
};

struct Slab slab_new(size_t etype);

bool slab_write(struct Slab *cont, size_t at, void *val);

void slab_move(struct Slab *cont, size_t i_from, size_t i_to);

void *slab_read(struct Slab *cont, size_t at);

bool slab_resize(struct Slab *cont, size_t capacity);

void slab_drop(struct Slab *cont);

#endif
