#include "interpreter.h"
#ifdef ENABLE_INTERPRETER
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
    intrp.vars = vec_new(sizeof(struct Interpreter_Variable));
    vec_push(&intrp.vars, &(struct Interpreter_Variable){ "print", {IT_CFUNC, .data.cfunc.func = cfunc_print} });
}

void intrp_deinit() {
    vec_drop(&intrp.vars);
}

static struct Interpreter_Variable* find_var(char* ident, usize len) {

    for (usize i = 0; i < intrp.vars.size; i += 1) {
        struct Interpreter_Variable* currv = vec_get(&intrp.vars, i);
        if (!strncmp(currv->name, ident, len))
            return currv;
    }

    return NULL;
    
}

static struct Interpreter_Value eval(struct Parser_Node* node) {
    const string source = parser_get_state()->lexer.src;

    struct Interpreter_Value ret = void_val();

    switch (node->kind) {
        case PN_PROC:
            ret.type = IT_FUNC;
            ret.data.func.body = &node->children;
        break;
        case PN_STRING:
            ret.type = IT_STRING;
            ret.data.string.str = strndup(source + node->data.string.val.from + 1, node->data.string.val.size - 2);
        break;
        case PN_NUMBER:
            ret.type = IT_INT;
            ret.data.intg.val = atoi(source + node->data.string.val.from);
        break;
        case PN_IDENT: {
            struct Interpreter_Variable* var = find_var(source + node->data.ident.val.from, node->data.ident.val.size);
            ret = var->val;
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
    const string source = parser_get_state()->lexer.src;

    struct Interpreter_Variable* func = find_var(source + node->data.call.name.from, node->data.call.name.size);

    struct Vec args = vec_new(sizeof(struct Interpreter_Value));
    for (usize i = 0; i < node->children.size; i += 1) {
        struct Interpreter_Value val = eval(vec_get(&node->children, i));
        vec_push(&args, &val);
    }

    struct Interpreter_Value ret = void_val(); 
    switch (func->val.type) {
        case IT_CFUNC:
            ret = func->val.data.cfunc.func(&args);
        break;
        case IT_FUNC:
            ret = execute(func->val.data.func.body);
        break;
        default:
        break;
    }

    vec_drop(&args);
    return ret;
}

static void declaration(struct Parser_Node* node) {
    const string source = parser_get_state()->lexer.src;
    
    struct Parser_Node* object = vec_get(&node->children, 0);

    struct Interpreter_Variable var = {
        .name = strndup(source + node->data.decl.name.from, node->data.decl.name.size),
        .val = eval(object)
    };
    vec_push(&intrp.vars, &var);   
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
    return execute(find_var("main", 4)->val.data.func.body);
}
#endif
