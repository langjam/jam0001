#include "../../aid/arena/arena.h"
#include "../lib/test.h"
#include <stdlib.h>
#include <time.h>

void test_arena()
{
    TEST_BEGIN;
    struct Arena arena = arena_new(sizeof(int));
    srand((unsigned int)time(NULL));

    EXPECT(arena_is_free(&arena, 0) == true, "Newly made arena is free")
    EXPECT(
        arena_is_free(&arena, 324234) == true,
        "Out-of-bounds Arena access is free")


    int test_value = rand();
    INFO_MANUAL(printf(
        "Adding 2 values into Arena container, first is %d", test_value));
    usize key = arena_insert(&arena, &test_value);
    int test_value_2 = rand();
    INFO_MANUAL(printf("second is %d", test_value_2));
    usize key2 = arena_insert(&arena, &test_value_2);

    EXPECT(
        arena_is_free(&arena, key) == false,
        "Modified Arena should not have first element free")
    EXPECT(
        arena_is_free(&arena, key2) == false,
        "Modified Arena should not have second element free")
    EXPECT(
        *(int *)arena_get(&arena, key) == test_value,
        "Value for first element")
    EXPECT(
        *(int *)arena_get(&arena, key2) == test_value_2,
        "Value for second element")
    INFO_MANUAL(printf("Removing `key` (%ld)", key));
    arena_free_index(&arena, key);
    EXPECT(
        arena_is_free(&arena, key) == true,
        "Modified Arena should not have first element free");
    int another_value = rand();
    usize key3 = arena_insert(&arena, &another_value);
    EXPECT(
        *(int *)arena_get(&arena, key3) == another_value,
        "Value for replacement element");
    EXPECT(key == key3, "The keys of replaced values must match up");
    arena_drop(&arena);
}
