#include "types.h"
#include <string.h>

extern inline void dmpstrn(
        const char *t,
        usize n,
        int (*printer)(const char *fmt, ...)
);

inline void dmpstrn(
    const char *t,
    usize n,
    int (*printer)(const char *fmt, ...)
) {
    usize len = strlen(t) > n ? n : strlen(t);
    printer("\"");
    for (usize i = 0; i < len; i += 1) {
        if (t[i] == '\n') 
            printer("\\n");
        else
            printer("%c", t[i]);
    }
    printer("\"\n");
}
