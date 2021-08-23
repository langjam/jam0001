#include "eh.h"
#include <stdio.h>
// Error handling

static string filename = NULL;
static sb_t builder;

static string ansi(const string s) {
    return s;
}

static struct Span getlinespn(const usize line, const string src) {
    usize lf = 0;
    usize i;
    for (i = 0; src[i]; i += 1) {
        if (lf == line) break;
        if (src[i] == '\n') lf += 1;
    }
    usize linesize;
    for (linesize = 0; src[linesize+i]; linesize += 1) {
        if (src[linesize+i] == '\n' || src[linesize+i] == '\r') break;
    }
    return (struct Span) { .from = i, .size = linesize };
}

static void getlinefrompos(usize pos, const string src, usize *line, usize *col) {
    usize ln = 0;
    usize cl = 0;
    *line = 0;
    *col = 0;
    usize i;
    for (i = 0; src[i] && i < pos; i += 1) {
        cl += 1;
        if (src[i] == '\n') {
            cl = 0;
            ln += 1;
        }
    }    
    *line = ln;
    *col = cl;
}

void eh_set_file(const string file) {
    filename = file;
}

void eh_at_line(usize line, const string src) {
    struct Span spn = getlinespn(line, src);
    fprintf(stderr, "%s %6zu |%s %.*s\n", ansi("\x1b[1;36m"), line+1, ansi("\x1b[1;0m"), SPAN_PF(spn, src));
}

void eh_point(usize col) {
    for (usize i = 0; i < col+10; i += 1)
        fprintf(stderr, " ");
    fprintf(stderr, "%s^%s\n", ansi("\x1b[1;31m"), ansi("\x1b[0m"));
}

void eh_error(usize line, usize col, const string src) {
    const string file = filename != NULL ? filename : "(file unknown)";
    fprintf(stderr, "%serror%s: %s\n", ansi("\x1b[1;31m"), ansi("\x1b[0m"), sbend(&builder));
    fprintf(stderr, "        %s| %s:%zu:%zu\n%s", ansi("\x1b[1;36m"), file, line+1, col+1, ansi("\x1b[1;0m"));
    fprintf(stderr, "        %s|-----------------------%s\n", ansi("\x1b[1;36m"), ansi("\x1b[1;0m"));
    if (line > 1) eh_at_line(line-2, src);
    if (line > 0) eh_at_line(line-1, src);
    eh_at_line(line, src);
    eh_point(col);
    // Hack
    builder.size = 0;
}

void eh_error_pos(usize pos, const string src) {
    usize line, col;
    getlinefrompos(pos, src, &line, &col);
    eh_error(line, col, src);
}

void eh_init() {
    builder = sbstart();
}

void eh_deinit() {
    builder = sbstart();
}

sb_t* eh_messagesb() { 
    return &builder;
}

void eh_at_token(struct Token *tok, const string src) {
    eh_at_line(tok->line, src);
}


