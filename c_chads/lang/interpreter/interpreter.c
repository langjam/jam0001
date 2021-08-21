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
                printf("%.*s", (int)arg->data.string.str.size, arg->data.string.str.view);
            break;
            case IT_VOID:
                printf("(void)");
            break;
            case IT_CFUNC:
                printf("(cfunc)");
            break;
            case IT_FUNC:
                printf("(func)");
            break;
        }

        printf("\t");
    }
    printf("\n");

    return void_val();
}

void intrp_init() {
    intrp.global = map_new(sizeof(struct Interpreter_Value));
    intrp.vars = &intrp.global;

    map_add(intrp.vars, strview_from("print"), &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_print });
}

void intrp_deinit() {
    map_drop(&intrp.global);
}

static struct Interpreter_Value* get_var(strview_t name) {
    struct Interpreter_Value* val = map_get(intrp.vars, name);
    if (!val) 
        val = map_get(&intrp.global, name);
    return val;
}

static struct Interpreter_Value eval(struct Parser_Node* node) {
    struct Interpreter_Value ret = void_val();

    switch (node->kind) {
        case PN_PROC:
            ret.type = IT_FUNC;
            ret.data.func.body = &node->children;
        break;
        case PN_STRING:
            ret.type = IT_STRING;
            ret.data.string.str = strview_dup(node->data.string.val);
        break;
        case PN_NUMBER:
            ret.type = IT_INT;
            ret.data.intg.val = atoi(node->data.string.val.view);
        break;
        case PN_IDENT: {
            ret = *get_var(node->data.ident.val);
        } break;
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

    struct Interpreter_Value* func = get_var(node->data.call.name);

    struct Vec args = vec_new(sizeof(struct Interpreter_Value));
    for (usize i = 0; i < node->children.size; i += 1) {
        struct Interpreter_Value val = eval(vec_get(&node->children, i));
        vec_push(&args, &val);
    }

    struct Map* oldframe = intrp.vars;
    struct Map vars = map_new(sizeof(struct Vec));
    intrp.vars = &vars;

    struct Interpreter_Value ret = void_val(); 
    switch (func->type) {
        case IT_CFUNC: {
            ret = func->data.cfunc.func(&args);
        } break;
        case IT_FUNC:
            ret = execute(func->data.func.body);
        break;
        default:
        break;
    }

    vec_drop(&args);

    map_drop(intrp.vars);
    intrp.vars = oldframe;
    return ret;
}

static enum Interpreter_Type resolve_type(strview_t name) {
    if (strview_eq(name, strview_from("int")))    return IT_INT;    else 
    if (strview_eq(name, strview_from("string"))) return IT_STRING;

    return IT_VOID;
}

static void declaration(struct Parser_Node* node) {
    struct Interpreter_Value var = {
        .type = resolve_type(node->data.decl.type)
    };
    map_add(intrp.vars, node->data.decl.name, &var);   
}

static struct Interpreter_Value assign(struct Parser_Node* node) {
    struct Parser_Node* dst = vec_get(&node->children, 0); 
    strview_t dstname;
    if (dst->kind == PN_DECL) {
        declaration(dst);
        dstname = dst->data.decl.name;
    } else
        dstname = dst->data.ident.val;

    struct Interpreter_Value val = eval(vec_get(&node->children, 1));
    struct Interpreter_Value *var = get_var(dstname);
    if (val.type != var->type && var->type != IT_VOID)
        printf("Spank Spank! Bad Programmer! Mismatched type\n");

    return (*var = val);
}

struct Interpreter_Value intrp_run(struct Parser_Node* node) {
    
    struct Interpreter_Value ret = void_val(); 
    switch (node->kind) {
        case PN_TOPLEVEL:
            ret = execute(&node->children);
        break;
        case PN_ASSIGN:
            ret = assign(node);
        break;
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
    return execute(((struct Interpreter_Value*)map_get(&intrp.global, strview_from("main")))->data.func.body);
}
