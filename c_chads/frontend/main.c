#include <stdio.h>
#include <stdlib.h>
#include "../lang/parser/parser.h"
#include "../lang/interpreter/interpreter.h"
#include "../aid/sfio/sfio.h"

void print_ast(struct Parser_Node *node, usize depth) {
    for (usize i = 0; i < depth; i += 1) 
        printf("    ");
    switch (node->kind) {
        case PN_TOPLEVEL:
            printf("Toplevel\n");
            break;
        case PN_DECL:
            printf("Decl(name = %.*s)\n", (int)node->data.decl.name.size, node->data.decl.name.view);
            break;
        case PN_PROC:
            printf("Proc\n");
            break;
        case PN_CALL:
            printf("Call(name = %.*s)\n", (int)node->data.call.name.size, node->data.call.name.view);
            break;
        case PN_STRING:
            printf("String(%.*s)\n", (int)node->data.string.val.size, node->data.string.val.view);
            break;
        case PN_NUMBER:
            printf("Number(%.*s)\n", (int)node->data.number.val.size, node->data.number.val.view);
            break;
        case PN_IDENT:
            printf("Ident(%.*s)\n", (int)node->data.ident.val.size, node->data.ident.val.view);
            break;
    }
    for (usize i = 0; i < node->children.size; i += 1) {
        print_ast(vec_get(&node->children, i), depth + 1);
    }
    // just temp
#ifndef ENABLE_INTERPRETER
    vec_drop(&node->children);
#endif
}

int main(int argc, char *argv[]) {
    if (argc < 2)  {
        fprintf(stderr, "No input files provided, usage: %s <file>\n", argv[0]);
        return EXIT_FAILURE;
    }
    const string src = sfio_read_text(argv[1]);
    if (src == NULL) {
        fprintf(stderr, "Can't access file %s\n", argv[1]);
        return EXIT_FAILURE;
    } 
    eh_set_file(argv[1]);
    eh_init();
    parser_init(src);
    printf("------\n%s\n------\n", parser_get_state()->lexer.src);
    struct Parser_Node result = parser_parse_toplevel();
    print_ast(&result, 0);
    printf("------Running------\n");

#ifdef ENABLE_INTERPRETER
    intrp_init();

    intrp_run(&result);
    intrp_main();

    intrp_deinit();
#endif

    printf("------\n");

    parser_deinit();
    free(src);
    eh_deinit();
    return 0;
}
