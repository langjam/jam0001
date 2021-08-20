#include "iter.h"

static void derive(struct ArenaIter *iter) 
{
    iter->done = arena_is_free(iter->src, iter->index);
    if (arena_iter_going(iter))
        iter->el = arena_get(iter->src, iter->index);
}

struct ArenaIter arena_iter_begin(struct Arena *src)
{
    struct ArenaIter me = {
        .src = src,
        .index = arena_first_index(src),
    };

    derive(&me);
    return me;
}

bool arena_iter_going(struct ArenaIter *iter)
{
    return !iter->done;
}

void arena_iter_next(struct ArenaIter *iter)
{
    if (arena_iter_going(iter))
    {
        iter->index = arena_advance_index(iter->src, iter->index);
        derive(iter);
    }
}


