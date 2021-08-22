#include "checker.h"
#include "../../aid/map/map.h"

typedef struct Parser_Node pnode_t;
typedef struct Parser_State pstate_t;
typedef struct Token tok_t;
typedef enum Parser_Node_Kind pnode_kind_t;

struct Map OF(struct Decl) env;

static void handle_macro(pnode_t *node) {
    if (node->children.size == 0) {
        eh_error_pos(node->pos, "Empty comment macro");
        exit(1);
    }
    for (usize i = 1; i < node->children.size; i += 1) {

    }
}

static void rec_checker(pnode_t *node) {
    struct Vec OF(strview_t) decls = vec_new(sizeof(strview_t));
    switch (node->kind) {
        case PN_DECL:
            if (map_get(&env, node->data.decl.name) != NULL) {
                eh_error_pos(node->pos, "Variable shadows previous declaration");
                exit(1);
            }   
            map_add(&env, node->data.decl.name, node);
            vec_push(&decls, &node->data.decl.name);
            for (usize i = 0; i < node->children.size; i += 1) {
                handle_macro(vec_get(&node->children, i));
            }
        break;
        case PN_MACRO:
            for (usize i = 0; i < node->children.size; i += 1) {
                handle_macro(vec_get(&node->children, i));
            }
        break;
        default: 
            for (usize i = 0; i < node->children.size; i += 1) {
                rec_checker(vec_get(&node->children, i));
            }
    }
    for (usize i = 0; i < decls.size; i += 1) {
        map_remove(&env, *(strview_t*)vec_get(&decls, i));
    }
} 

void checker_run(pnode_t *node) {
    env = map_new(sizeof(struct Parser_Node));
    rec_checker(node);
}
