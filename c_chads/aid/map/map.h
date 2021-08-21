#include "../vec/vec.h"
#include "../common/prelude.h"
#include "../strview/strview.h"

struct Map {
    struct Vec OF(strview_t) keys;
    struct Vec OF(void) values;
};

struct Map map_new(usize el_size);
void map_insert(struct Map *self, strview_t key, void *value);
void* map_get(struct Map *self, strview_t key);
void map_remove(struct Map *self, strview_t key);
void map_drop(struct Map *self);
