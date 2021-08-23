#include "strview.h"
#include <string.h>

strview_t strview_from(string str) {
    return (strview_t) {
        .view = str,
        .size = strlen(str)
    };
}

strview_t strview_dup(strview_t view) {
    view.view = strndup(view.view, view.size);
    return view;
}

strview_t strview_span(struct Span span, string str) {
    return (strview_t) {
        .view = str+span.from,
        .size = span.size
    };
}

strview_t strview_fromdup(string str) {
    return (strview_t) {
        .view = strdup(str),
        .size = strlen(str)
    };
}

strview_t strview_spandup(struct Span span, string str) {
    return (strview_t) {
        .view = strndup(str+span.from, span.size),
        .size = span.size
    };
}

int strview_cmp(strview_t a, strview_t b) {
    if (a.size > b.size) return 1;
    if (a.size < b.size) return -1;
    return strncmp(a.view, b.view, a.size); 
}


bool strview_eq(strview_t a, strview_t b) {
    return strview_cmp(a, b) == 0; 
}
