#pragma once
#include "arena.h"
#define ARENA_ITER(it, src) (struct ArenaIter it = arena_iter_begin(src); arena_iter_going(&it); arena_iter_next(&it))

struct ArenaIter 
{
    bool done;
    struct Arena *src;
    usize index;
    void *el;
};


// Iterator stuff
struct ArenaIter arena_iter_begin(struct Arena *src);
bool arena_iter_going(struct ArenaIter *iter);
void arena_iter_next(struct ArenaIter *iter);


