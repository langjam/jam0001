#include "interpreter.h"
#include "../eh.h"

static struct Interpreter_State intrp;

static const char* type_names[] = {
    [IT_VOID] = "void",
    [IT_CFUNC] = "cfunc",
    [IT_FUNC] = "func",
    [IT_INT] = "int",
    [IT_FLOAT] = "float",
    [IT_STRING] = "string"
};

static void error_tok(strview_t tok) {
    const string src = parser_get_state()->lexer.src;
    eh_error_pos((usize)(tok.view - src), src);
    exit(1);
}

static void error_mmtype(enum Interpreter_Type expected, enum Interpreter_Type supplied, strview_t tok) {

    EH_MESSAGE("Mismatched type: '%s' expected but '%s' supplied", type_names[expected], type_names[supplied]);
    error_tok(tok);
}

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
            case IT_FLOAT:
                printf("%f", arg->data.flt.val);
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

static struct Interpreter_Value cfunc_int(struct Vec OF(struct Interpreter_Value) *args) {
    struct Interpreter_Value val = VEC_GET_VAL(struct Interpreter_Value, args, 0);
    val.data.intg.val = (int)val.data.flt.val;
    val.type = IT_INT;
    return val;
}

static struct Interpreter_Value cfunc_float(struct Vec OF(struct Interpreter_Value) *args) {
    struct Interpreter_Value val = VEC_GET_VAL(struct Interpreter_Value, args, 0);
    val.data.flt.val = (float)val.data.intg.val;
    val.type = IT_FLOAT;
    return val;
}

void intrp_init() {
    intrp.global = map_new(sizeof(struct Interpreter_Value));
    intrp.vars = &intrp.global;

    map_add(intrp.vars, strview_from("print"), &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_print });
    map_add(intrp.vars, strview_from("int"),   &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_int   });
    map_add(intrp.vars, strview_from("float"), &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_float });
}

void intrp_deinit() {
    map_drop(&intrp.global);
}

static struct Interpreter_Value* get_var(strview_t name) {
    struct Interpreter_Value* val = map_get(intrp.vars, name);
    if (!val) 
        val = map_get(&intrp.global, name);

    if (!val) {
        EH_MESSAGE("Variable '%.*s' not found", (int)name.size, name.view);
        error_tok(name);
    }

    return val;
}

static struct Interpreter_Value execute(struct Parser_Node* body, bool* should_return) {

    struct Interpreter_Value retval = void_val();

    for (usize i = 0; i < body->children.size; i++) {
        bool sr;
        retval = intrp_run(vec_get(&body->children, i), &sr);

        if (sr) {
            if (should_return) *should_return = sr;
            return retval;
        }
    }

    return retval;
}

static struct Interpreter_Value call(struct Parser_Node* node) {

    struct Interpreter_Value* func = get_var(pnode_left(node)->data.ident.val);

    struct Map* oldframe = intrp.vars;
    struct Map vars = map_new(sizeof(struct Vec));

    struct Vec* args = &pnode_right(node)->children;

    struct Interpreter_Value ret = void_val(); 
    switch (func->type) {
        case IT_CFUNC: {

            struct Vec params = vec_new(sizeof(struct Interpreter_Value));
            for (usize i = 0; i < args->size; i += 1) {
                struct Interpreter_Value arg = intrp_run(vec_get(args, i), NULL);
                vec_push(&params, &arg);
            }

            intrp.vars = &vars;
            ret = func->data.cfunc.func(&params);
            vec_drop(&params);
        } break;
        case IT_FUNC: {
            struct Vec* params = &pnode_left(func->data.func.ast)->children;
            for (usize i = 0; i < args->size; i++) {
                struct Interpreter_Value arg = intrp_run(vec_get(args, i), NULL);
                struct Parser_Node* fdecl = vec_get(params, i);
                map_add(&vars, fdecl->data.decl.name, &arg);
            }
            intrp.vars = &vars;
            ret = execute(pnode_right(func->data.func.ast), NULL);
        } break;
        default:
        break;
    }

    for (usize i = 0; i < intrp.vars->values.size; i++) {
        struct Interpreter_Value* var = vec_get(&intrp.vars->values, i);
        switch (var->type) {
        case IT_STRING:
            free(var->data.string.str.view);
        break;
        default:
        break;
        }
    }
    map_drop(intrp.vars);
    intrp.vars = oldframe;
    return ret;
}

static enum Interpreter_Type resolve_type(strview_t name) {
    if (strview_eq(name, strview_from("int")))    return IT_INT;    else 
    if (strview_eq(name, strview_from("float")))  return IT_FLOAT;  else 
    if (strview_eq(name, strview_from("string"))) return IT_STRING;

    return IT_VOID;
}

static void declaration(struct Parser_Node* node) {
    struct Interpreter_Value var = {
        .type = resolve_type(node->data.decl.type.name)
    };
    map_add(intrp.vars, node->data.decl.name, &var);   
}

static struct Interpreter_Value assign(struct Parser_Node* node) {
    struct Parser_Node* dst = pnode_left(node); 
    strview_t dstname;
    if (dst->kind == PN_DECL) {
        declaration(dst);
        dstname = dst->data.decl.name;
    } else
        dstname = dst->data.ident.val;

    struct Interpreter_Value val = intrp_run(pnode_right(node), NULL);
    struct Interpreter_Value *var = get_var(dstname);
    if (val.type != var->type && var->type != IT_VOID)
        error_mmtype(var->type, val.type, dstname);

    return (*var = val);
}

struct Interpreter_Value intrp_run(struct Parser_Node* node, bool* should_return) {
    if (should_return) *should_return = false;
    
    struct Interpreter_Value ret = void_val(); 
    switch (node->kind) {
        case PN_TOPLEVEL: case PN_BODY:
            ret = execute(node, should_return);
        break;
        case PN_ASSIGN:
            ret = assign(node);
        break;
        case PN_CALL:
            ret = call(node);                        
        break;
        case PN_RETURN:
            ret = intrp_run(pnode_uvalue(node), should_return);
            *should_return = true;
        break;
        case PN_IF: {

            struct Interpreter_Value cond = intrp_run(node->addressing == PA_BINARY ? pnode_left(node) : pnode_cond(node), should_return);
            if (cond.type != IT_INT)
                error_mmtype(IT_INT, cond.type, (strview_t){0});

            if (node->addressing == PA_BINARY) {
                if (cond.data.intg.val)
                    ret = execute(pnode_right(node), should_return);
            } else {
                if (cond.data.intg.val)
                    ret = intrp_run(pnode_body(node), should_return);
                else
                    ret = intrp_run(pnode_alt(node), should_return);

            }

        } break;
        case PN_WHILE:
            while (1) {
                struct Interpreter_Value cond = intrp_run(pnode_left(node), should_return);
                if (cond.data.intg.val)
                    ret = execute(pnode_right(node), should_return);
                else break;
            }
        break;
        case PN_DECL:
            declaration(node);
        break;
        case PN_PROC:
            ret.type = IT_FUNC;
            ret.data.func.ast = node;
        break;
        case PN_STRING:
            ret.type = IT_STRING;
            ret.data.string.str = (strview_t){
                .view = strndup(node->data.string.val.view+1, node->data.string.val.size-2),
                .size = node->data.string.val.size-2
            };
        break;
        case PN_NUMBER:

            if (node->data.number.kind != PNM_FLT) {
                ret.type = IT_INT;
                int base, offs;

                switch (node->data.number.kind) {
                case PNM_HEX: base = 16; offs = 2; break;
                case PNM_BIN: base = 2;  offs = 2; break;
                case PNM_OCT: base = 8;  offs = 1; break;
                default:      base = 10; offs = 0; break;
                }

                ret.data.intg.val = (int)strtol(node->data.number.val.view + offs, NULL, base);
            } else {
                ret.type = IT_FLOAT;
                ret.data.flt.val = strtof(node->data.number.val.view, NULL);
            }
        break;
        case PN_IDENT: {
            ret = *get_var(node->data.ident.val);
        } break;
        case PN_OPERATOR: {

            struct Interpreter_Value 
                left = intrp_run(pnode_left(node), NULL),
                right = intrp_run(pnode_right(node), NULL);

            if (left.type != right.type)
                error_mmtype(left.type, right.type, node->data.op.op);

            if (left.type == IT_INT) {
                ret.type = IT_INT;
                int* rp = &ret.data.intg.val;
                if (node->data.op.op.view[1] != '=') {
                    int lv = left.data.intg.val, rv = right.data.intg.val;

                    switch (node->data.op.op.view[0]) {
                    case '+': *rp = lv + rv; break;
                    case '-': *rp = lv - rv; break;
                    case '*': *rp = lv * rv; break;
                    case '/': *rp = lv / rv; break;
                    case '%': *rp = lv % rv; break;

                    case '<': if (node->data.op.op.view[1] == '<') *rp = lv << rv; else *rp = lv < rv; break;
                    case '>': if (node->data.op.op.view[1] == '>') *rp = lv >> rv; else *rp = lv > rv; break;
                    case '&': *rp = lv & rv; break;
                    case '|': *rp = lv | rv; break;
                    case '^': *rp = lv ^ rv; break;
                    }
                } else {
                    int *lv = &left.data.intg.val, *rv = &right.data.intg.val;
                    switch (node->data.op.op.view[0]) {
                    case '+': *rp = *lv += *rv; break;
                    case '-': *rp = *lv -= *rv; break;
                    case '*': *rp = *lv *= *rv; break;
                    case '/': *rp = *lv /= *rv; break;
                    case '%': *rp = *lv %= *rv; break;

                    case '>': *rp = *lv >= *rv; break;
                    case '<': *rp = *lv <= *rv; break;
                    case '=': *rp = *lv == *rv; break;
                    case '!': *rp = *lv != *rv; break;
                    }
                }
            } else if (left.type == IT_FLOAT) {
                ret.type = IT_FLOAT;
                float* rp = &ret.data.flt.val;
                if (node->data.op.op.view[1] != '=') {
                    float lv = left.data.flt.val, rv = right.data.flt.val;
                    switch (node->data.op.op.view[0]) {
                    case '+': *rp = lv + rv; break;
                    case '-': *rp = lv - rv; break;
                    case '*': *rp = lv * rv; break;
                    case '/': *rp = lv / rv; break;

                    case '<': *rp = lv < rv; break;
                    case '>': *rp = lv > rv; break;
                    }
                } else {
                    float *lv = &left.data.flt.val, *rv = &right.data.flt.val;
                    switch (node->data.op.op.view[0]) {
                    case '+': *rp = *lv += *rv; break;
                    case '-': *rp = *lv -= *rv; break;
                    case '*': *rp = *lv *= *rv; break;
                    case '/': *rp = *lv /= *rv; break;

                    case '>': *rp = *lv >= *rv; break;
                    case '<': *rp = *lv <= *rv; break;
                    case '=': *rp = *lv == *rv; break;
                    }
                }
                
            } else {
                EH_MESSAGE("Operator '%.*s' doesn't support the operands '%s' and '%s'", (int)node->data.op.op.size, node->data.op.op.view, type_names[left.type], type_names[right.type]);
                error_tok(node->data.op.op);
            }

        } break;
        case PN_UNARY: {

            struct Interpreter_Value val = intrp_run(pnode_uvalue(node), NULL);

            if (node->data.unary.op.view[0] == '!') {
                if (val.type != IT_INT) {
                    error_mmtype(IT_INT, val.type, node->data.unary.op);
                } else val.data.intg.val = !val.data.intg.val;
            } else if (node->data.unary.op.view[0] == '-') {
                if (val.type == IT_INT)
                    val.data.intg.val = -val.data.intg.val;
                else if (val.type == IT_FLOAT)
                    val.data.flt.val = -val.data.flt.val;
                else {
                    EH_MESSAGE("Operator '-' doesn't support operand of type '%s'", type_names[val.type]);
                    error_tok(node->data.unary.op);
                }

            }

            ret = val;
        } break;
        default:
        break;
    }

    return ret;
}

struct Interpreter_Value intrp_main() {
    return execute(pnode_right(get_var(strview_from("main"))->data.func.ast), NULL);
}
