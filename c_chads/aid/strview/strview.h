// String view
#pragma once

#include "../common/prelude.h"
#include "../span/span.h"


struct String_View {
    string view;
    usize size;
}
typedef strview_t;

strview_t strview_from(string str);
strview_t strview_dup(strview_t view);
strview_t strview_span(struct Span span, string str);
strview_t strview_fromdup(string str);
strview_t strview_spandup(struct Span span, string str);
int strview_cmp(strview_t a, strview_t b);
bool strview_eq(strview_t a, strview_t b);
