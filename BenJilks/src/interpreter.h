#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "parser.h"
#include "scope.h"

typedef struct _Interpreter
{
    Heap *heap;
    Scope global_scope;
} Interpreter;

void interpreter_run(Body*);

#endif // INTERPRETER_H

