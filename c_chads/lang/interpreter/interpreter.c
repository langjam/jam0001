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

static void error_pos(usize pos) {
    const string src = parser_get_state()->lexer.src;
    eh_error_pos(pos, src);
    exit(1);
}

static usize get_pos(strview_t tok) {
    return (usize)(tok.view - parser_get_state()->lexer.src);
}

static void error_mmtype(enum Interpreter_Type expected, enum Interpreter_Type supplied, usize pos) {

    EH_MESSAGE("Mismatched type: '%s' expected but '%s' supplied", type_names[expected], type_names[supplied]);
    error_pos(pos);
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
            case IT_ARRAY:
                for (usize i = 0; i < arg->data.array.values.size; i++) {
                    struct Vec tv = vec_new(sizeof(struct Interpreter_Value));
                    vec_push(&tv, vec_get(&arg->data.array.values, i));
                    cfunc_print(&tv);
                    vec_drop(&tv);
                    printf(", ");
                }
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
            case IT_CPTR:
                printf("(cptr)");
            break;
            case IT_STRUCT:
                printf("(struct)");
            break;
            case IT_STRUCTDECL:
                printf("(structdecl)");
            break;
        }

        printf(" ");
    }

    return void_val();
}

static struct Interpreter_Value cfunc_read(struct Vec OF(struct Interpreter_Value) *args) {
    (void)args;

    char* buf = malloc(256);
    fgets(buf, 256, stdin);

    return (struct Interpreter_Value){.type = IT_STRING, .data.string.str = { buf, strlen(buf)-1 }};
}

static struct Interpreter_Value cfunc_readf(struct Vec OF(struct Interpreter_Value) *args) {
    (void)args;

    float val;
    scanf("%f", &val);

    return (struct Interpreter_Value){.type = IT_FLOAT, .data.flt.val = val};
}

static struct Interpreter_Value cfunc_len(struct Vec OF(struct Interpreter_Value) *args) {
    struct Interpreter_Value* val = vec_get(args, 0);
    if (val->type != IT_ARRAY)
        error_mmtype(IT_ARRAY, val->type, intrp.cfunc_call_pos);

    return (struct Interpreter_Value){.type = IT_INT, .data.intg.val = (int)val->data.array.values.size};
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
    map_add(intrp.vars, strview_from("read"),  &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_read  });
    map_add(intrp.vars, strview_from("readf"), &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_readf});
    map_add(intrp.vars, strview_from("len"),   &(struct Interpreter_Value){ IT_CFUNC, .data.cfunc.func = cfunc_len});
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
        error_pos(get_pos(name));
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

static void val_drop(struct Interpreter_Value* val) {
    switch (val->type) {
    case IT_STRING:
        free(val->data.string.str.view);
    break;
    case IT_ARRAY:
        for (usize i = 0; i < val->data.array.values.size; i++)
            val_drop(vec_get(&val->data.array.values, i));
        vec_drop(&val->data.array.values);
    break;
    case IT_STRUCTDECL:
        map_drop(&val->data.strctdecl.fields);
    break;
    case IT_STRUCT:
        map_drop(&val->data.strct.fields);
    break;
    default:
    break;
    }
}

static struct Interpreter_Value call(struct Parser_Node* node) {

    struct Interpreter_Value* func = get_var(pnode_left(node)->data.ident.val);

    struct Map* oldframe = intrp.vars;
    struct Map vars = map_new(sizeof(struct Vec));

    struct Vec* args = &pnode_right(node)->children;

    struct Interpreter_Value ret = void_val(); 
    switch (func->type) {
        case IT_CFUNC: {
            intrp.cfunc_call_pos = node->pos;

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
            intrp.rettype = func->data.func.rettype;

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

    for (usize i = 0; i < intrp.vars->values.size; i++)
        val_drop(vec_get(&intrp.vars->values, i));
    map_drop(intrp.vars);

    intrp.vars = oldframe;
    return ret;
}

static enum Interpreter_Type resolve_type(strview_t name) {
    if (name.view[0] == ':') return IT_ARRAY;
    if (strview_eq(name, strview_from("int")))    return IT_INT;    else 
    if (strview_eq(name, strview_from("float")))    return IT_INT;    else 
    if (strview_eq(name, strview_from("struct"))) return IT_STRUCT;    else 
    if (strview_eq(name, strview_from("float")))  return IT_FLOAT;  else 
    if (strview_eq(name, strview_from("string"))) return IT_STRING;
    if (strview_eq(name, strview_from("_"))) return IT_VOID;
    else {
        EH_MESSAGE("Unknown type: '%.*s'", (int)name.size, name.view);
        error_pos(get_pos(name));    
    }

    return IT_VOID;
}

static void declaration(struct Parser_Node* node) {
    struct Interpreter_Value var = {
        .type = resolve_type(node->data.decl.type.name)
    };
    map_add(intrp.vars, node->data.decl.name, &var);   
}

static struct Interpreter_Value* faccess(struct Parser_Node* node) {
    struct Interpreter_Value array = intrp_run(pnode_left(node), NULL);
    struct Interpreter_Value index = intrp_run(pnode_right(node), NULL);
    if (array.type != IT_ARRAY)
        error_mmtype(IT_ARRAY, index.type, pnode_left(node)->pos);
    if (index.type != IT_INT)
        error_mmtype(IT_INT, index.type, pnode_right(node)->pos);

    int idx = index.data.intg.val;
    if (idx < 0) idx = (int)array.data.array.values.size + idx;

    return vec_get(&array.data.array.values, (usize)idx);
}

static struct Interpreter_Value* field(struct Parser_Node* node) {
    struct Interpreter_Value strct = intrp_run(pnode_uvalue(node), NULL);
    strview_t field = node->data.field.name; 
    if (strct.type != IT_STRUCT)
        error_mmtype(IT_STRUCT, strct.type, node->pos);

    struct Interpreter_Value* val = map_get(&strct.data.strct.fields, field);
    if (!val) {
        EH_MESSAGE("Field '%.*s' not found in struct", (int)field.size, field.view);
        error_pos(get_pos(field));
    }

    return val;
}

static struct Interpreter_Value assign(struct Parser_Node* node) {
    struct Parser_Node* dst = pnode_left(node); 
    struct Interpreter_Value* dstval = NULL;
    if (dst->kind == PN_DECL) {
        declaration(dst);
        dstval = get_var(dst->data.decl.name);
    } else if (dst->kind == PN_ACCESS)
        dstval = faccess(dst);
    else if (dst->kind == PN_FIELD)
        dstval = field(dst);
    else {
        EH_MESSAGE("Cannot assign to value");
        error_pos(dst->pos);
    }

    struct Interpreter_Value val = intrp_run(pnode_right(node), NULL);
    if (val.type != dstval->type && dstval->type != IT_VOID)
        error_mmtype(dstval->type, val.type, dst->pos);

    return (*dstval = val);
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
            if (ret.type != intrp.rettype)
                error_mmtype(intrp.rettype, ret.type, pnode_uvalue(node)->pos);

            *should_return = true;
        break;
        case PN_IF: {

            struct Parser_Node* cond_node = node->addressing == PA_BINARY ? pnode_left(node) : pnode_cond(node);
            struct Interpreter_Value cond = intrp_run(cond_node, should_return);
            if (cond.type != IT_INT)
                error_mmtype(IT_INT, cond.type, cond_node->pos);

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
            ret.data.func.rettype = resolve_type(node->data.proc.return_type.name);
            ret.data.func.ast = node;
        break;
        case PN_STRUCT:
            ret.type = IT_STRUCTDECL;
            ret.data.strctdecl.fields = map_new(sizeof(enum Interpreter_Type));
           
            for (usize i = 0; i < node->children.size; i++) {
                struct Parser_Node* fdecl = vec_get(&node->children, i);
                enum Interpreter_Type type = resolve_type(fdecl->data.decl.type.name);
                map_add(&ret.data.strctdecl.fields, fdecl->data.decl.name, &type);
            }
        break;
        case PN_NEW:
            ret.type = IT_STRUCT;
            ret.data.strct.typename = node->data.newinst.type.name;
            ret.data.strct.fields = map_new(sizeof(struct Interpreter_Value));

            for (usize i = 0; i < node->children.size; i++) {
                struct Parser_Node* init = vec_get(&node->children, i);
                struct Interpreter_Value val = intrp_run(pnode_uvalue(init), should_return);
                map_add(&ret.data.strct.fields, init->data.init.name, &val);
            }

        break;
        case PN_FIELD: {
            return *field(node);
        } break;
        case PN_STRING:
            ret.type = IT_STRING;
            ret.data.string.str = (strview_t){
                .view = strndup(node->data.string.val.view+1, node->data.string.val.size-2),
                .size = node->data.string.val.size-2
            };
        break;
        case PN_LIST:
            ret.type = IT_ARRAY;
            ret.data.array.values = vec_new(sizeof(struct Interpreter_Value));
            for (usize i = 0; i < node->children.size; i++) {
                struct Interpreter_Value val = intrp_run(vec_get(&node->children, i), should_return);
                vec_push(&ret.data.array.values, &val);
            }
        break;
        case PN_ACCESS: {
            ret = *faccess(node);
        } break;
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
                error_mmtype(left.type, right.type, get_pos(node->data.op.op));

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

                    case '<': ret.type = IT_INT; ret.data.intg.val = lv < rv; break;
                    case '>': ret.type = IT_INT; ret.data.intg.val = lv > rv; break;
                    }
                } else {
                    float *lv = &left.data.flt.val, *rv = &right.data.flt.val;
                    switch (node->data.op.op.view[0]) {
                    case '+': *rp = *lv += *rv; break;
                    case '-': *rp = *lv -= *rv; break;
                    case '*': *rp = *lv *= *rv; break;
                    case '/': *rp = *lv /= *rv; break;

                    case '<': ret.type = IT_INT; ret.data.intg.val = *lv <= *rv; break;
                    case '>': ret.type = IT_INT; ret.data.intg.val = *lv >= *rv; break;
                    case '=': ret.type = IT_INT; ret.data.intg.val = *lv == *rv; break;
                    }
                }
                
            } else {
                EH_MESSAGE("Operator '%.*s' doesn't support the operands '%s' and '%s'", (int)node->data.op.op.size, node->data.op.op.view, type_names[left.type], type_names[right.type]);
                error_pos(node->pos);
            }

        } break;
        case PN_UNARY: {

            struct Interpreter_Value val = intrp_run(pnode_uvalue(node), NULL);

            if (node->data.unary.op.view[0] == '!') {
                if (val.type != IT_INT) {
                    error_mmtype(IT_INT, val.type, node->pos);
                } else val.data.intg.val = !val.data.intg.val;
            } else if (node->data.unary.op.view[0] == '-') {
                if (val.type == IT_INT)
                    val.data.intg.val = -val.data.intg.val;
                else if (val.type == IT_FLOAT)
                    val.data.flt.val = -val.data.flt.val;
                else {
                    EH_MESSAGE("Operator '-' doesn't support operand of type '%s'", type_names[val.type]);
                    error_pos(node->pos);
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
