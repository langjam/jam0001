#include "interpreter.h"

static struct Interpreter_State intrp;

static struct Interpreter_Value void_val() {
    return (struct Interpreter_Value){ IT_VOID, {0} };
}

#include <stdio.h>
static struct Interpreter_Value cfunc_print(struct Vec OF(struct Interpreter_Value) *args) {

    for (usize i = 0; i < args->size; i++) {
        struct Interpreter_Value* arg = vec_get(args, i);

        switch (arg->type) {
            case IT_INT:
                printf("%d", arg->data.intg.val);
            break;
            case IT_STRING:
                printf("%s", arg->data.string.str);
            break;
            case IT_VOID:
                printf("(void)");
            break;
        }

        printf("\t");
    }
    printf("\n");

    return void_val();
}

void intrp_init() {
    intrp.funcs = vec_new(sizeof(struct Interpreter_Function));
    vec_push(&intrp.funcs, &(struct Interpreter_Function){ "print", ITF_CFUNC, .data.cfunc.func = cfunc_print });
}

void intrp_deinit() {
    vec_drop(&intrp.funcs);
}

static struct Interpreter_Value eval(const string source, struct Parser_Node* node) {

    struct Interpreter_Value ret = void_val();

    switch (node->kind) {
        case PN_STRING:
            ret.type = IT_STRING;
            ret.data.string.str = strndup(source + node->data.string.val.from + 1, node->data.string.val.size - 2);
        break;
        case PN_NUMBER:
            ret.type = IT_INT;
            ret.data.intg.val = atoi(source + node->data.string.val.from);
        break;
        default:
        break;
    }

    return ret;
}

static struct Interpreter_Value execute(struct Vec OF(struct Parser_Node*) *body) {

    for (usize i = 0; i < body->size; i++)
        intrp_run(vec_get(body, i));

    return void_val();
}

static struct Interpreter_Value call(struct Parser_Node* node) {
    const string source = parser_get_state()->lexer.src;

    struct Interpreter_Function* func = NULL;
    for (usize i = 0; i < intrp.funcs.size; i += 1) {
        struct Interpreter_Function* currf = vec_get(&intrp.funcs, i);
        if (!strncmp(
            currf->name, 
            source + node->data.call.name.from,
            node->data.call.name.size)) {

            func = currf;
            break;        
        }
    }

    struct Vec args = vec_new(sizeof(struct Interpreter_Value));
    for (usize i = 0; i < node->children.size; i += 1) {
        struct Interpreter_Value val = eval(source, vec_get(&node->children, i));
        vec_push(&args, &val);
    }

    struct Interpreter_Value ret; 
    switch (func->kind) {
        case ITF_CFUNC:
            ret = func->data.cfunc.func(&args);
        break;
        case ITF_NORMAL:
            ret = execute(func->data.normal.body);
        break;
    }

    vec_drop(&args);
    return ret;
}

static void declaration(struct Parser_Node* node) {
    const string source = parser_get_state()->lexer.src;
    
    struct Parser_Node* object = vec_get(&node->children, 0);
    switch (object->kind) {
        case PN_PROC: {
            struct Interpreter_Function func = {
                .name = strndup(source + node->data.decl.name.from, node->data.decl.name.size),
                .kind = ITF_NORMAL,
                .data.normal.body = &object->children
            };
            vec_push(&intrp.funcs, &func);
        }
        break;
        default:
        break;
    }
}

struct Interpreter_Value intrp_run(struct Parser_Node* node) {
    //const string source = parser_get_state()->lexer.src;
    
    struct Interpreter_Value ret = void_val(); 
    switch (node->kind) {
        case PN_CALL:
            ret = call(node);                        
        break;
        case PN_DECL:
            declaration(node);
        break;
        default:
        break;
    }

    return ret;
}

struct Interpreter_Value intrp_main() {

    for (usize i = 0; i < intrp.funcs.size; i += 1) {
        struct Interpreter_Function* currf = vec_get(&intrp.funcs, i);
        if (!strcmp(currf->name, "main"))
            return execute(currf->data.normal.body);
    }
    
    return void_val();
}
