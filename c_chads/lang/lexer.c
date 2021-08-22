#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
#define TRY_SKIP(o, c) if (!lex_skip_rune(o, c)) return TT_INVALID;
 
const string TT_NAMES[256] = {
    [TT_EOF] = "Eof",
    [TT_INVALID] = "Invalid",
    [TT_STRING] = "String",
    [TT_HEXADECIMAL] = "Number",
    [TT_BINARY] = "Binary",
    [TT_OCTAL] = "Octal",
    [TT_INTEGER] = "Integer",
    [TT_FLOATING] = "Floating",
    [TT_IDENT] = "Ident",
    [TT_DEF] = "Definition",
    [TT_LBRACE] = "LeftBrace",
    [TT_RBRACE] = "RightBrace",
    [TT_LPAREN] = "LeftParen",
    [TT_RPAREN] = "RightParen",
    [TT_LBRACKET] = "LeftBracket",
    [TT_RBRACKET] = "RightBracket",
    [TT_OPERATOR] = "Op",
    [TT_COMMA] = ",",
    [TT_SEMI] = "Semi",
    [TT_RETURN] = "Return",
    [TT_PROC] = "Proc",
    [TT_NEW] = "New",
};
 
struct Lexer_State lex_new(const string input) {
    return (struct Lexer_State) {
        .pos = 0,
        .line = 0,
        .col = 0,
        .src_len = strlen(input),
        .src = strdup(input)
    };
}
 
void lex_drop(struct Lexer_State *self) {
    free((rune*)self->src);
}
 
static bool lex_eof(struct Lexer_State *self) {
    return self->pos >= self->src_len;
}
 
static struct Token invalid_token() {
    return (struct Token) { .tt = TT_INVALID };
}
 
__attribute__((unused))
static rune peek_ahead(struct Lexer_State *self, usize of) {
    if ((self->pos+of) < self->src_len) 
        return self->src[self->pos+of];
    else  {
        return '\0';
    }
}
 
static rune lex_peek(struct Lexer_State *self) {
    if (self->pos < self->src_len) 
        return self->src[self->pos];
    else  {
        return '\0';
    }
}
 
static bool lex_is(struct Lexer_State *self, rune ch) {
    rune chek = lex_peek(self);
    return chek == ch;
}
 
static rune lex_skip(struct Lexer_State *self) {
    rune ch = lex_peek(self);
    if (ch != '\0') {
        self->pos += 1;
        self->col += 1;
    }
    if (ch == '\n') {
        self->line += 1;
        self->col = 0;
    }
    return ch;
}
 
static bool lex_skip_rune(struct Lexer_State *self, rune ch) {
    if (lex_is(self, ch)) {
        lex_skip(self);
        return true;
    }
    return false;
}
 
static enum Token_Type lex_str(struct Lexer_State *self) {
    TRY_SKIP(self, '"');
    rune ch;
    while (ch = lex_skip(self), ch != '"') {
        if (ch == '\0')
            return TT_EOF;
        else if (ch == '\\') lex_skip(self);
    }
    return TT_STRING;
}
 
static bool is_idu(rune ch) {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
}
 
static bool is_num(rune ch) {
    return ch >= '0' && ch <= '9';
}
 
static enum Token_Type lex_ident(struct Lexer_State *self) {
    rune ch = lex_skip(self);
    if (!is_idu(ch)) return TT_INVALID;
    while (ch = lex_peek(self), is_num(ch) || is_idu(ch)) lex_skip(self);
    return TT_IDENT;
}
 
static enum Token_Type lex_num(struct Lexer_State *self) {
    if (!(is_num(lex_peek(self)) || lex_peek(self) == '.')) return TT_INVALID;

    if (lex_peek(self) == '0') {
        rune ch;
        lex_skip(self);
        if (lex_peek(self) == 'x') {
            lex_skip(self);
            while (ch = lex_peek(self), (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')) lex_skip(self);
            return TT_HEXADECIMAL;
        }
        else if (lex_peek(self) == 'b') {
            lex_skip(self);
            while (ch = lex_peek(self), (ch >= '0' && ch <= '1')) lex_skip(self);
            return TT_BINARY;
        }
        else {
            while (ch = lex_peek(self), (ch >= '0' && ch <= '7')) lex_skip(self);
            return TT_OCTAL;
        }
    }

    enum Token_Type tt = TT_INTEGER;
    bool had_dot = false;

    rune ch;
    while (ch = lex_peek(self), is_num(ch) || (ch == '.')) {
        if (ch == '.') {
            if (had_dot) break;
            had_dot = true;
            tt = TT_FLOATING;
        }
        lex_skip(self);
    }
    
    return tt;
}
 
static enum Token_Type lex_single_rune(struct Lexer_State *self) {
    switch (lex_skip(self)) {
        case ':':
            return TT_DEF;
        case ',':
            return TT_COMMA;
        case '{':
            return TT_LBRACE;
        case '}':
            return TT_RBRACE;
        case '[':
            return TT_LBRACKET;
        case ']':
            return TT_RBRACKET;
        case '!':
        case '<':
            if (lex_peek(self) == '<')
            {
                lex_skip(self);
            }
        case '>':
            if (lex_peek(self) == '>')
            {
                lex_skip(self);
            }
        case '=':
            if (lex_peek(self) == '=')
            {
                lex_skip(self);
            }
        case '&':
            if (lex_peek(self) == '&')
            {
                lex_skip(self);
            }
        case '|':
            if (lex_peek(self) == '|')
            {
                lex_skip(self);
            }
        case '^':
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
            return TT_OPERATOR;
        case '(':
            return TT_LPAREN;
        case ')':
            return TT_RPAREN;
        case ';':
            return TT_SEMI;
    }
    return TT_INVALID;
}

static enum Token_Type check_for_keyword(struct Lexer_State *self, struct Token *tok) {
    if (spanstreqstr(tok->span, self->src, "return")) {
        return TT_RETURN;
    }
    if (spanstreqstr(tok->span, self->src, "proc")) {
        return TT_PROC;
    }
    if (spanstreqstr(tok->span, self->src, "struct")) {
        return TT_STRUCT;
    }
    if (spanstreqstr(tok->span, self->src, "new")) {
        return TT_NEW;
    }
    return TT_INVALID;
}

static struct Token lex_try(struct Lexer_State *self, const Lexer_Function lexfn) {
    usize startpos = self->pos;
    usize startline = self->line;
    usize startcol = self->col;
    enum Token_Type tt = lexfn(self);
    if (tt != TT_INVALID && startpos != self->pos) {
        struct Token tok = (struct Token) {
            .span = { .from = startpos, .size = self->pos-startpos},
            .tt = tt
        };
        // Convienient spot to intercept the kewords
        enum Token_Type newtt = check_for_keyword(self, &tok);
        if (newtt != TT_INVALID) {
            tok.tt = newtt;
        }
        return tok;
    }
    else {
        // Rewind
        struct Token tok = invalid_token();
        tok.span.from = self->pos-1;
        tok.span.size = self->pos-startpos;
        self->pos = startpos;
        self->col = startcol;
        self->line = startline;
        return tok;
    }
}   
 
static bool lex_skip_blank(struct Lexer_State *self) {
    usize startpos = self->pos;
    rune ch;
    while (ch = lex_peek(self), ch == '\n' || ch == '\t' || ch == ' ' || ch == '\r')
        lex_skip(self);
    return self->pos != startpos;
}
 
static bool lex_skip_comment(struct Lexer_State *self) {
    usize startpos = self->pos;
    if (lex_peek(self) == '#') {
        lex_skip(self);
        rune ch;
        while (ch = lex_peek(self), !(ch == '\n' || ch == '\r'))
            lex_skip(self);
    }
    return self->pos != startpos;
 
}
 
static void lex_skip_stuff(struct Lexer_State *self) {
    while (lex_skip_blank(self) || lex_skip_comment(self));
}

struct Span lex_get_comment(struct Lexer_State *self) {
    while (lex_skip_blank(self));   
    usize startpos = self->pos;
    lex_skip_comment(self);
    usize size = self->pos-startpos;
    // Empty span, means no comment exists
    if (size == 0) return (struct Span) { 0 };
    return (struct Span) { .from = startpos+1, .size = size-1 };
}

struct Token lex_determine(struct Lexer_State *self) {
    lex_skip_stuff(self);
    usize tok_line = self->line;
    usize tok_col = self->col;

    // Starting line/col pair for token
    const Lexer_Function lexfn[] = {
        lex_num, lex_str, lex_ident, lex_single_rune
    };
    const usize lexfn_count = sizeof(lexfn)/sizeof(Lexer_Function);
    struct Token tok = invalid_token();

    if (lex_eof(self)) {
        tok.tt = TT_EOF; 
        goto end;
    }

    for (usize i = 0; i < lexfn_count; i += 1) {
        tok = lex_try(self, lexfn[i]);
        if (tok.tt != TT_INVALID) break;
    }
end:
    // This is needed, because INVALID unwinds, while others don't
    //if (tok.tt != TT_INVALID && tok.tt != TT_EOF && tok_col > 0)
    //    tok_col -= 1;

    tok.line = tok_line;
    tok.col = tok_col;
    return tok;
}
