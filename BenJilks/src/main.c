#include <stdio.h>
#include <stdlib.h>
#include "parser.h"
#include "preprocess.h"
#include "allocator.h"
#include "interpreter.h"

// #define DEBUG

int main(int argc, char **argv)
{
    // Determain source file
    FILE *file_in;
    if (argc == 2)
        file_in = fopen(argv[1], "r");
    else
        file_in = stdin;

    // Preprocess
    char *source = NULL;
    size_t source_len = 0;
    if (preprocess(file_in, &source, &source_len) != 0)
        return 1;

    // Parse
    yyin = fmemopen(source, source_len, "r");
    if (yyparse() != 0)
        return 1;

    if (did_error)
        goto cleanup;

#ifdef DEBUG
    // Debug
    printf("Preprocessed source:\n%s\n", source);
    printf("AST:\n");
    debug_log_program(parsed_program);

    // Run
    printf("\nProgram Output:\n");
#endif
    interpreter_run(parsed_program);

    // Clean up
cleanup:
    allocator_free_all();
    fclose(yyin);
    free(source);
    if (argc == 2)
        fclose(file_in);
    return 0;
}

