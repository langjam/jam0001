#pragma once
#include "../parser/parser.h"
#include "../../aid/map/map.h"

enum Interpreter_Type {
    IT_VOID,
    IT_CFUNC,
    IT_FUNC,
    IT_INT,
    IT_FLOAT,
    IT_STRING,
    IT_ARRAY
};

typedef struct Interpreter_Value (*Interpreter_Cfunc)(struct Vec OF(struct Interpreter_Value)*);

struct Interpreter_Value {
    enum Interpreter_Type type;
    union {
        struct {
            struct Parser_Node* ast;
        } func;
        struct {
            Interpreter_Cfunc func;
        } cfunc;
        struct {
            int val;
        } intg;
        struct {
            float val;
        } flt;
        struct {
            strview_t str;
        } string;
        struct {
            struct Vec OF(struct Interperter_Value) values;
        } array;
    } data;
};

struct Interpreter_State {
    struct Map OF(struct Interpreter_Value) global;
    struct Map OF(struct Interpreter_Value) *vars;
};

void intrp_init();
void intrp_deinit();
struct Interpreter_Value intrp_run(struct Parser_Node*, bool* should_return);
struct Interpreter_Value intrp_main();
