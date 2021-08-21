#include "lexer.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static void print_node(struct Node *node);

static void print_help(void) {
    printf("./ech --file [file]\n");
    printf("./ech --string \"string of code\"\n");
}

static char *load_file(const char* const filename) {
    FILE *file;
    char *allocated;
    size_t size;
    if (!(file = fopen(filename, "r")))
        return NULL;
    fseek(file, 0, SEEK_END);
    size = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (!(allocated = malloc((size+1) * sizeof(char))))
        return NULL;
    fread(allocated, size, 1, file);
    allocated[size] = '\0';
    fclose(file);
    return allocated;
}

static void print_value(struct Value *value) {
    switch (value->type) {
    case ValueTypeVoid:
        printf("(value: (type: void))");
        break;
    case ValueTypeNumber:
        printf("(value: (type: number, value: %d))", value->as.number);
        break;
    case ValueTypeString:
        printf("(value: (type: string, value: \"%s\"))", value->as.string);
        break;
    case ValueTypeRoutine:
        printf("(value: (type: routine, parameter_names: (");
        if (value->as.routine.amount_parameters > 0) {
            printf("%s", value->as.routine.parameters[0]);
            for (size_t i = 1; i < value->as.routine.amount_parameters; ++i) {
                printf(", %s", value->as.routine.parameters[i]);
            }
        }
        printf("), nodes: (");

        for (size_t i = 0; value->as.routine.block[i]; ++i) {
            print_node(value->as.routine.block[i]);
            printf(", ");
        }
        printf(")))");
        break;
    default:
        assert(0);
    }
}

static void print_expr(struct Expr *expr) {
    switch (expr->type) {
    case ExprTypeAdd:
        printf("(op: '+', lhs: ");
        print_expr(expr->as.binary.lhs);
        printf(", rhs: ");
        print_expr(expr->as.binary.rhs);
        printf(")");
        break;
    case ExprTypeSub:
        printf("(op: '-', lhs: ");
        print_expr(expr->as.binary.lhs);
        printf(", rhs: ");
        print_expr(expr->as.binary.rhs);
        printf(")");
        break;
    case ExprTypeMul:
        printf("(op: '*', lhs: ");
        print_expr(expr->as.binary.lhs);
        printf(", rhs: ");
        print_expr(expr->as.binary.rhs);
        printf(")");
        break;
    case ExprTypeDiv:
        printf("(op: '/', lhs: ");
        print_expr(expr->as.binary.lhs);
        printf(", rhs: ");
        print_expr(expr->as.binary.rhs);
        printf(")");
        break;
    case ExprTypeKey:
        printf("(key: %s)", expr->as.key);
        break;
    case ExprTypeValue:
        print_value(expr->as.value);
        break;
    default:
        assert(0);
    }
}

static void print_node(struct Node *node) {
    switch (node->type) {
    case NodeTypeLog:
        printf("(log: ");
        print_expr(node->value.log_value);
        printf(")");
        break;
    case NodeTypeSet:
        printf("(set: (key: %s, value: ", node->value.set.key);
        print_expr(node->value.set.expr);
        printf("))");
        break;
    default:
        //FIXME: wtf?
        break;
    }
}

int main(int argc, char **argv) {
    char *stream_base;
    bool do_free = false;
    --argc; ++argv;
    if (argc == 0) {
        print_help();
        return 1;
    }
    if (strcmp(argv[0], "--string") == 0 || strcmp(argv[0], "-s") == 0) {
        if (argc == 1)
            goto not_enough_arguments;
        if (!(stream_base = argv[1]))
            return 0;
    } else if (strcmp(argv[0], "--file") == 0 || strcmp(argv[0], "-f") == 0) {
        if (argc == 1)
            goto not_enough_arguments;
        if (!(stream_base = load_file(argv[1]))) {
            perror(argv[1]);
            return 1;
        }
        do_free = true;
    } else {
        print_help();
        return 1;
    }
    for (struct Node **node = parse(stream_base); *node; ++node) {
        print_node(*node);
        printf("\n");
    }
    if (do_free)
        free(stream_base);
    return 0;
not_enough_arguments:
    fprintf(stderr, "Expected an input string after '%s' flag\n", argv[0]);
    return 1;
}
