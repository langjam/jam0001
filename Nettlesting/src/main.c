#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nettle.h"

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("usage: nsting [source]\n");
        return 1;
    }
    char *src = argv[1];
    
    FILE *in = fopen(src, "r");
    
    long size = 0;
    fseek(in, 0, SEEK_END);
    size = ftell(in);
    rewind(in);
    
    char *arr = malloc(size + 1);
    memset(arr, 0, size);

    fread(arr, 1, size, in);
    nettle(arr, size);
    
    free(arr);
    fclose(in);

    return 0;
}