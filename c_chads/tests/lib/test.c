#include <stdio.h>
#include <stdlib.h>

#include "test.h"

int total_count = 0;
int total_passed = 0;

static struct TestNode {
    char const *name;

    int total_count;
    int passed_count;
} test_node = { NULL, 0, 0 };


void __test_begin(char const *name)
{
    if (test_node.name != NULL) {
        printf("\x1b[37;1mtest finished for %s (%d/%d)\x1b[0m\n", test_node.name, test_node.passed_count, test_node.total_count);
    }
    test_node.name = name;
    test_node.passed_count = 0;
    test_node.total_count = 0;
    if (name != NULL)
        printf("\x1b[37;1m%s\x1b[0m\n", name);
}

void __test_end()
{
    __test_begin(NULL);
    printf("\x1b[37;1mtesting done: (%d/%d)\x1b[0m\n", total_passed, total_count);
}

void __test_expect(
    int line, char const *file, char const *condition_repr, char const *message, int result)
{
    (void) line;
    (void) file;
    test_node.total_count += 1;
    total_count += 1;
    test_node.passed_count += result ? 1 : 0;
    total_passed += result ? 1 : 0;
    printf(
        "    %s \x1b[34m%d\x1b[0m: \x1b[33m%s\x1b[0m\n",
        result ? "\x1b[32;1mpass\x1b[0m" : "\x1b[31;1mfail\x1b[0m",
        line,
        condition_repr);
    if (message)
        printf("    \x1b[37m^ %s\x1b[0m\n", message);
}

void __test_info_man(int line)
{
    printf("    \x1b[36;1minfo\x1b[0m \x1b[34m%d\x1b[0m: ", line);
}
