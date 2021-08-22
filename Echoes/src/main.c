#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static size_t indent;

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

void print_indent(void) {
    for (size_t i = 0; i < indent; ++i) {
        printf("  ");
    }
}

static void print_value(struct Value *value) {
    switch (value->type) {
    case ValueTypeVoid:
        print_indent(); printf("value\n");
        ++indent;
        print_indent(); printf("type\n");
        ++indent;
        print_indent(); printf("void\n");
        indent -= 2;
        break;
    case ValueTypeNumber:
        print_indent(); printf("value\n");
        ++indent;
        print_indent(); printf("type\n");
        ++indent;
        print_indent(); printf("number\n");
        --indent;
        print_indent(); printf("value\n");
        ++indent;
        print_indent();
        if (value->as.number.is_float)
            printf("%f\n", value->as.number.as._float);
        else
            printf("%d\n", value->as.number.as._int);
        indent -= 2;
        break;
    case ValueTypeString:
        print_indent(); printf("value\n");
        ++indent;
        print_indent(); printf("type\n");
        ++indent;
        print_indent(); printf("number\n");
        --indent;
        print_indent(); printf("value\n");
        ++indent;
        print_indent(); printf("\"%s\"\n", value->as.string);
        indent -= 2;
        break;
    case ValueTypeRoutine:
        print_indent(); printf("value\n");
        ++indent;
        print_indent(); printf("type\n");
        ++indent;
        print_indent(); printf("routine\n");
        --indent;
        print_indent(); printf("parameter_names\n");
        ++indent;
        for (size_t i = 0; i < value->as.routine.amount_parameters; ++i) {
            print_indent(); printf(":%s\n", value->as.routine.parameters[i]);
        }
        --indent;
        print_indent(); printf("nodes\n");
        ++indent;
        for (size_t i = 0; value->as.routine.block[i]; ++i) {
            print_node(value->as.routine.block[i]);
        }
        indent -= 2;
        break;
    default:
        assert(0);
    }
}

static void print_expr(struct Expr *expr) {
    switch (expr->type) {
    case ExprTypeAdd:
        print_indent(); printf("+\n");
        break;
    case ExprTypeSub:
        print_indent(); printf("-\n");
        break;
    case ExprTypeMul:
        print_indent(); printf("*\n");
        break;
    case ExprTypeDiv:
        print_indent(); printf("/\n");
        break;
    case ExprTypeEquals:
        print_indent(); printf("=\n");
        break;
    case ExprTypeSmallerThen:
        print_indent(); printf("<\n");
        break;
    case ExprTypeBiggerThen:
        print_indent(); printf(">\n");
        break;
    case ExprTypeKey:
        print_indent(); printf("key\n");
        ++indent;
        print_indent(); printf(":%s\n", expr->as.key);
        --indent;
        return;
    case ExprTypeValue:
        print_value(expr->as.value);
        return;
    default:
        assert(0);
    }
    ++indent;
    print_indent(); printf("lhs\n");
    ++indent;
    print_expr(expr->as.binary.lhs);
    --indent;
    print_indent(); printf("rhs\n");
    ++indent;
    print_expr(expr->as.binary.rhs);
    indent -= 2;
}

static void print_node(struct Node *node) {
    switch (node->type) {
    case NodeTypeLog:
        print_indent(); printf("log\n");
        ++indent;
        print_expr(node->value.log_value);
        --indent;
        break;
    case NodeTypeSet:
        print_indent(); printf("set\n");
        ++indent;
        print_indent(); printf("key\n");
        ++indent;
        print_indent(); printf(":%s\n", node->value.set.key);
        --indent;
        print_expr(node->value.set.expr);
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
    interpret(parse(stream_base));
    if (do_free)
        free(stream_base);
    return 0;
not_enough_arguments:
    fprintf(stderr, "Expected an input string after '%s' flag\n", argv[0]);
    return 1;
}
