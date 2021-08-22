#ifndef HEAP_H
#define HEAP_H

typedef struct _Object Object;
typedef struct _Heap Heap;
typedef struct _Visitor Visitor;

Heap *heap_new();
void heap_free(Heap*);

Object *heap_allocate_object(Heap*);
Object *heap_allocate_list(Heap*, int len);

void heap_visit(Object*);
void heap_do_garbage_collection(Heap*);

#endif // HEAP_H

