#include "varmap.h"
#include "parser.h"

struct Block {
    size_t depth;
    struct VariableMap vars;
};

struct Interpreter {
    struct Node **instructions;
    size_t idx;
    struct Block block;
};

void interpret(struct Node **instructions);
