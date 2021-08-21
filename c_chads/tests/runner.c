#include "lib/test.h"
#include "tests/test_arena.c"
#include "tests/test_vec.c"
#include "tests/test_lexer.c"
#include "tests/test_map.c"

int main()
{
    test_arena();
    test_vec();
    test_lexer();
    test_map();
    __test_end();
}
