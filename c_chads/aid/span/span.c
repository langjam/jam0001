#include "span.h"
#include "../common/types.h"
#include <stdlib.h>
#include <string.h>

bool spanstreqstr(struct Span span, const char *spanned, const char *basic) {
    if (strlen(basic) != span.size) return false;
    return strncmp(spanned+span.from, basic, span.size) == 0;
}

bool spanstreqspanstr(struct Span span, const char *spanned, struct Span span2, const char *spanned2) {
    if (span.size != span2.size) return false;
    spanned += span.from;
    spanned2 += span2.from;
    for (usize i = 0; i < span.size; i += 1) {
        if (spanned[i] != spanned2[i]) return false;
    }

    return true;
}
