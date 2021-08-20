#pragma once
#include <stdbool.h>
#include <stddef.h>

struct Span {
    size_t from;
    size_t size;
};


bool spanstreqstr(struct Span span, const char *spanned, const char *basic);
bool spanstreqspanstr(struct Span span, const char *spanned, struct Span span2, const char *spanned2);
