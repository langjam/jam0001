#pragma once
#include "../parser/parser.h"

enum Interpreter_Type {
    IT_VOID,
    IT_CFUNC,
    IT_FUNC,
    IT_INT,
    IT_STRING
};

typedef struct Interpreter_Value (*Interpreter_Cfunc)(struct Vec OF(struct Interpreter_Value)*);

struct Interpreter_Value {
    enum Interpreter_Type type;
    union {
        struct {
            struct Vec OF(struct Parser_Node*) *body;
        } func;
        struct {
            Interpreter_Cfunc func;
        } cfunc;
        struct {
            int val;
        } intg;
        struct {
            char* str;
        } string;
    } data;
};

struct Interpreter_Variable {
    string name;
    struct Interpreter_Value val;
};

struct Interpreter_State {
    struct Vec OF(struct Interpreter_Variable) vars;
};

void intrp_init();
void intrp_deinit();
struct Interpreter_Value intrp_run(struct Parser_Node*);
struct Interpreter_Value intrp_main();
