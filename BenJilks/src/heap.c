#include "heap.h"
#include "object.h"
#include <stdlib.h>
#include <memory.h>
#include <assert.h>

#define MIN_BLOCK_SIZE 128
// #define DEBUG_HEAP

#define MAX(a, b) (a) > (b) ? (a) : (b)

typedef struct _HeapBlock
{
    Object *slots;
    int marked;

    int size;
    int used;
    int freed;
    struct _HeapBlock *next;
    struct _HeapBlock *next_free;
    Heap *heap;
} HeapBlock;

struct _Heap
{
    HeapBlock *root;
    HeapBlock *first_free;
};

static HeapBlock *create_block(Heap *heap, int size)
{
    HeapBlock *block = malloc(sizeof(HeapBlock));
    block->slots = malloc(sizeof(Object) * size);
    block->marked = 0;
    block->size = size;
    block->used = 0;
    block->freed = 0;
    block->next = NULL;
    block->next_free = NULL;
    block->heap = heap;

#ifdef DEBUG_HEAP
    printf("Allocate new heap block 0x%lx of size %d\n", (unsigned long)block, size);
#endif
    return block;
}

Heap *heap_new()
{
    Heap *heap = malloc(sizeof(Heap));
    heap->root = create_block(heap, MIN_BLOCK_SIZE);
    heap->first_free = heap->root;
    return heap;
}

static void free_block(HeapBlock *block)
{
    for (int i = 0; i < block->used; i++)
        object_free(&block->slots[i]);
}

void heap_free(Heap *heap)
{
    HeapBlock *curr = heap->root;
    while(curr)
    {
        HeapBlock *next = curr->next;
        free_block(curr);
        free(curr->slots);
        free(curr);
        curr = next;
    }
}

Object *heap_allocate_list(Heap *heap, int len)
{
    HeapBlock *block = heap->first_free;
    while (block->size - block->used < len)
    {
        if (!block->next_free)
        {
            block->next_free = create_block(heap, MAX(MIN_BLOCK_SIZE, len));
            block->next_free->next = heap->root;
            heap->root = block->next_free;
        }
        block = block->next_free;
    }

    Object *object = &block->slots[block->used];
    object->allocator = (void*)block;
    block->used += len;

    assert(block->used <= block->size);
    if (block->used == block->size)
    {
        heap->first_free = block->next_free;
        if (!heap->first_free)
        {
            HeapBlock *new_block = create_block(heap, MIN_BLOCK_SIZE);
            new_block->next = heap->root;
            heap->root = new_block;
            heap->first_free = new_block;
        }
    }
    return object;
}

Object *heap_allocate_object(Heap *heap)
{
    return heap_allocate_list(heap, 1);
}

static void free_objects_in_block(HeapBlock *block, int count)
{
    if (count == 0)
        return;

    block->freed += count;

    // If the block is now empty, add it to free list
    assert(block->freed <= block->size);
    if (block->freed == block->size)
    {
        free_block(block);
        block->used = 0;
        block->freed = 0;
        block->next_free = block->heap->first_free;
        block->heap->first_free = block;

#ifdef DEBUG_HEAP
        printf("Block 0x%lx now empty, add to free list\n", (unsigned long)block);
#endif
    }
}

void heap_visit(Object *object)
{
    HeapBlock *block = (HeapBlock*)object->allocator;
    block->marked += 1;
}

void heap_do_garbage_collection(Heap *heap)
{
#ifdef DEBUG_HEAP
    int freed_count = 0;
#endif

    for (HeapBlock *it = heap->root; it; it = it->next)
    {
        int count_unmarked = it->used - it->freed - it->marked;
        free_objects_in_block(it, count_unmarked);
#ifdef DEBUG_HEAP
        freed_count += count_unmarked;
#endif

        it->marked = 0;
    }

#ifdef DEBUG_HEAP
    printf("Freed %i objects\n", freed_count);
#endif
}

