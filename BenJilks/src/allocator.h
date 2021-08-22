#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <stddef.h>

void *allocator_new(
    size_t size);

const char *allocator_string(
    const char *str);

const char *allocator_sub_string(
    const char *str, 
    int offset_from_start, 
    int offset_from_end);

void allocator_free_all();

#define ALLOCATE_TYPE(type) \
    (type*)allocator_new(sizeof(type))

#endif // ALLOCATOR_H

