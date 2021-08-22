#include "allocator.h"
#include <stdlib.h>
#include <string.h>

#define HEAP_BUFFER_SIZE 1024
#define MAX(a, b) (a) > (b) ? (a) : (b)

typedef struct _HeapBlock
{
    void *data;
    int size;
    int pointer;
    struct _HeapBlock *next;
} HeapBlock;

static HeapBlock *s_heap_root = NULL;
static HeapBlock *s_current_heap_block = NULL;

static HeapBlock *create_heap_block(int min_size)
{
    HeapBlock *block = (HeapBlock*)malloc(sizeof(HeapBlock));
    block->size = MAX(min_size, HEAP_BUFFER_SIZE);
    block->pointer = 0;
    block->data = malloc(block->size);
    block->next = NULL;
    return block;
}

void *allocator_new(
    size_t size)
{
    // If no blocks exist, make one
    if (s_current_heap_block == NULL)
    {
        s_heap_root = create_heap_block(size);
        s_current_heap_block = s_heap_root;
    }

    // If there's not enough data in the 
    // current block, create a new one.
    if (s_current_heap_block->pointer + size >= s_current_heap_block->size)
    {
        s_current_heap_block->next = create_heap_block(size);
        s_current_heap_block = s_current_heap_block->next;
    }

    // Calculate pointers
    void *pointer = s_current_heap_block->data + s_current_heap_block->pointer;
    s_current_heap_block->pointer += size;
    return pointer;
}

const char *allocator_string(
    const char *str)
{
    return allocator_sub_string(str, 0, 0);
}

const char *allocator_sub_string(
    const char *str, 
    int offset_from_start, 
    int offset_from_end)
{
    // Calculate lengths
    size_t len = strlen(str);
    size_t new_str_len = len - offset_from_start - offset_from_end;

    // Copy string into new data buffer
    char *new_str = (char*)allocator_new(new_str_len + 1);
    memcpy(new_str, str + offset_from_start, len - offset_from_end);

    // Terminate string
    new_str[new_str_len] = '\0';

    return new_str;
}

void free_heap_block(HeapBlock *block)
{
    if (block == NULL)
        return;

    free_heap_block(block->next);
    free(block->data);
    free(block);
}

void allocator_free_all()
{
    free_heap_block(s_heap_root);
    s_heap_root = NULL;
    s_current_heap_block = NULL;
}

