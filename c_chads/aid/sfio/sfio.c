#include "sfio.h"
#include <stdio.h>
#include <stdlib.h>

char* sfio_read_text(const char *path) 
{
    FILE *f = fopen(path, "r");
    if (f == NULL) 
        return NULL;
    fseek(f, 0, SEEK_END);
    size_t fsz = (size_t) ftell(f);
    fseek(f, 0, SEEK_SET);
    char *input = malloc((fsz+1)*sizeof(char));
    input[fsz] = 0;
    fread(input, sizeof(char), fsz, f);
    fclose(f);
    return input;
}

void sfio_write_text(const char *path, const char *text)
{
    FILE *f = fopen(path, "w+");
    fputs(text, f);
    fclose(f);
}
