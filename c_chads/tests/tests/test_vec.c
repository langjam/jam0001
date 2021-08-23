#include "../lib/test.h"
#include "../../aid/vec/vec.h"
#include "../../aid/common/prelude.h"

void test_vec()
{
    TEST_BEGIN;
    struct Vec vec = vec_new(sizeof(int));

    int v = 32;
    vec_push(&vec, &v);
    v = 64;
    vec_push(&vec, &v);
    EXPECT(
        *(int *)vec_get(&vec, 0) == 32,
        "0th value in the vector should be 32");
    EXPECT(
        *(int *)vec_get(&vec, 1) == 64,
        "1st value in the vector should be 64");

    EXPECT(
        *(int *)vec_get(&vec, vec.size - 1) == 64,
        "Top value in the vector should be 64");
    vec_pop(&vec);
    EXPECT(
        *(int *)vec_get(&vec, vec.size - 1) == 32,
        "Top value in the vector should be 32");
    vec_pop(&vec);

    EXPECT(vec.size == 0, "Size of the vector should be 0");

    EXPECT(
        vec_get(&vec, 0) == NULL,
        "0th value in the vector should be NULL after free");
    EXPECT(
        vec_get(&vec, 1) == NULL,
        "1st value in the vector should be NULL after free");
    EXPECT(
        vec_get(&vec, (usize)rand()) == NULL,
        "1st value in the vector should be NULL after free");
    INFO_MANUAL(printf("Testing moving"));
    v = 32;
    vec_push(&vec, &v);
    v = 64;
    vec_push(&vec, &v);
    v = 128;
    vec_push(&vec, &v);
    v = 256;
    vec_push(&vec, &v);
    INFO_MANUAL(printf("We have a vector [32, 64, 128, 256]"));
    vec_remove(&vec, 1);
    INFO_MANUAL(
        printf("Removing element 1, we should have [32, 128, 256]"));
    EXPECT(*(int *)vec_get(&vec, 0) == 32);
    EXPECT(*(int *)vec_get(&vec, 1) == 128);
    EXPECT(*(int *)vec_get(&vec, 2) == 256);
    vec_remove(&vec, 0);
    INFO_MANUAL(printf(
        "Ok, now removing element 0, we should have [128, 256]"));
    EXPECT(*(int *)vec_get(&vec, 0) == 128);
    EXPECT(*(int *)vec_get(&vec, 1) == 256);
    vec_remove(&vec, 1);
    INFO_MANUAL(
        printf("And, now removing element 1, we should have [128]"));
    EXPECT(*(int *)vec_get(&vec, 0) == 128);
    vec_drop(&vec);
}
