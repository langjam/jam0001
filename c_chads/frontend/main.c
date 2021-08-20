#include <stdio.h>

int main(int argc, char *argv[]) {
    for (int i = 1; i < argc; i += 1) {
        printf("Argument %s\n", argv[i]);
    }
    if (argc == 1) {
        printf("No arguments provided\n");
    }
    return 0;
}
