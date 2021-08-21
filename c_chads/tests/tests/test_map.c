#include "../lib/test.h"
#include "../../aid/map/map.h"

void test_map() {
    TEST_BEGIN;
    int val = 42;
    struct Map map = map_new(sizeof(int));
    map_add(&map, strview_from("hello"), &val);
    val = 0xC1A;
    map_add(&map, strview_from("world"), &val);

    EXPECT(*(int*)map_get(&map, strview_from("hello")) == 42); 
    EXPECT(*(int*)map_get(&map, strview_from("world")) == 0xC1A); 

    map_remove(&map, strview_from("hello"));
    EXPECT(map_get(&map, strview_from("hello")) == NULL); 
    EXPECT(*(int*)map_get(&map, strview_from("world")) == 0xC1A); 
    map_remove(&map, strview_from("world"));
    EXPECT(map_get(&map, strview_from("world")) == NULL); 
    map_drop(&map);
}
