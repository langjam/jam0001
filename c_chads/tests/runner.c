#include "lib/test.h"
#include "tests/test_arena.c"
#include "tests/test_vec.c"
#include "tests/test_lexer.c"

int main()
{
    test_arena();
    test_vec();
    test_lexer();
    __test_end();
}
