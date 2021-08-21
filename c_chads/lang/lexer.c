#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
#define TRY_SKIP(o, c) if (!lex_skip_rune(o, c)) return TT_INVALID;
 
const string TT_NAMES[256] = {
    [TT_EOF] = "Eof",
    [TT_INVALID] = "Invalid",
    [TT_STRING] = "String",
    [TT_NUMBER] = "Number",
    [TT_IDENT] = "Ident",
    [TT_DEF] = "Definition",
    [TT_ASSIGN] = "Assignment",
    [TT_LBRACE] = "LeftBrace",
    [TT_RBRACE] = "RightBrace",
    [TT_LPAREN] = "LeftParen",
    [TT_RPAREN] = "RightParen",
    [TT_LBRACKET] = "LeftBracket",
    [TT_RBRACKET] = "RightBracket",
    [TT_COMMA] = ",",
    [TT_LT] = "LessThan",
    [TT_GT] = "GreaterThan",
    [TT_SEMI] = "Semi",
    [TT_RETURN] = "Return",
    [TT_PROC] = "Proc"
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
    self->col += 1;
    if (ch == '\n') {
        self->line += 1;
        self->col = 0;
    }
    self->pos += 1;
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
            return TT_INVALID;
        else if (ch == '\\') lex_skip(self);
    }
    return TT_STRING;
}
 
static bool is_idu(rune ch) {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
}
 
static bool is_num(rune ch) {
    return ch > '0' && ch < '9';
}
 
static enum Token_Type lex_ident(struct Lexer_State *self) {
    rune ch = lex_skip(self);
    if (!is_idu(ch)) return TT_INVALID;
    while (ch = lex_peek(self), is_num(ch) || is_idu(ch)) lex_skip(self);
    return TT_IDENT;
}
 
static enum Token_Type lex_num(struct Lexer_State *self) {
 
    if (lex_is(self, '-')) TRY_SKIP(self, '-'); 
 
    rune ch = lex_skip(self);
    if (!is_num(ch)) return TT_INVALID;
    while (ch = lex_peek(self), ch >= '0' && ch <= '9') lex_skip(self);
    
    return TT_NUMBER;
}
 
static enum Token_Type lex_single_rune(struct Lexer_State *self) {
    switch (lex_skip(self)) {
        case ':':
            return TT_DEF;
        case ',':
            return TT_COMMA;
        case '=':
            return TT_ASSIGN;
        case '{':
            return TT_LBRACE;
        case '}':
            return TT_RBRACE;
        case '[':
            return TT_LBRACKET;
        case ']':
            return TT_RBRACKET;
        case '<':
            return TT_LT;
        case '>':
            return TT_GT;
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
 
struct Token lex_determine(struct Lexer_State *self) {
    lex_skip_stuff(self);
    const Lexer_Function lexfn[] = {
        lex_num, lex_str, lex_ident, lex_single_rune
    };
    const usize lexfn_count = sizeof(lexfn)/sizeof(Lexer_Function);
    if (lex_eof(self))
        return (struct Token) { .tt = TT_EOF };
    struct Token tok = invalid_token();
    for (usize i = 0; i < lexfn_count; i += 1) {
        tok = lex_try(self, lexfn[i]);
        if (tok.tt != TT_INVALID) break;
    }
    return tok;
}
