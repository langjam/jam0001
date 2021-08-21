#pragma once
#include "../parser/parser.h"

enum Interpreter_Type {
    IT_VOID,
    IT_INT,
    IT_STRING
};

struct Interpreter_Value {
    enum Interpreter_Type type;
    union {
        struct {
            int val;
        } intg;
        struct {
            char* str;
        } string;
    } data;
};

typedef struct Interpreter_Value (*Interpreter_Cfunc)(struct Vec OF(struct Interpreter_Value)*);

enum Interpreter_Function_Kind {
    ITF_CFUNC,
    ITF_NORMAL
};

struct Interpreter_Function {
    string name;
    enum Interpreter_Function_Kind kind; 
    union {
        struct {
            Interpreter_Cfunc func;
        } cfunc;
        struct {
            struct Vec OF(struct Parser_Node*) *body;
        } normal;
    } data;
};

struct Interpreter_Variable {
    string name;
    struct Interpreter_Value val;
};

struct Interpreter_State {
    struct Vec OF(struct Interpreter_Function) funcs;
    struct Vec OF(struct Interpreter_Variable) vars;
};

void intrp_init();
void intrp_deinit();
struct Interpreter_Value intrp_run(struct Parser_Node*);
struct Interpreter_Value intrp_main();