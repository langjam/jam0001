#pragma once
#include <stdbool.h>
#include <stddef.h>

// This is needed to prevent double-evaluation in macro below
extern _Thread_local struct Span _SPAN_TEMP;

#define SPAN_PF(s, src) (int)(_SPAN_TEMP = (s)).size, (_SPAN_TEMP).from+(src)

struct Span {
    size_t from;
    size_t size;
};


bool spanstreqstr(struct Span span, const char *spanned, const char *basic);
bool spanstreqspanstr(struct Span span, const char *spanned, struct Span span2, const char *spanned2);
