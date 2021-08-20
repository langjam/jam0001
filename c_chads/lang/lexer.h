#pragma once
#include <stddef.h>
#include <stdbool.h>
#include "../aid/span/span.h"
#include "../aid/common/prelude.h"

struct Lexer_State {
    usize pos;
    usize line;
    usize col;
    const usize src_len;
    const string src;
};

enum Token_Type {
    TT_EOF,
    TT_INVALID,
    TT_DEF,
    TT_ASSIGN,
    TT_STRING,
    TT_NUMBER,
    TT_IDENT,
    TT_LPAREN,
    TT_RPAREN,
    TT_LBRACE,
    TT_RBRACE,
    TT_LBRACKET,
    TT_RBRACKET,
    TT_LT,
    TT_GT,
    TT_SEMI,
    TT_RETURN
};

extern const string TT_NAMES[];

struct Token {
    enum Token_Type tt;
    struct Span span;
};

typedef enum Token_Type (*Lexer_Function)(struct Lexer_State*);

struct Lexer_State lex_new(const string input);
void lex_drop(struct Lexer_State *self);
struct Token lex_determine(struct Lexer_State *self);

