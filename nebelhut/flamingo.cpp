#include <stdio.h>
#include <stdlib.h>
#include "glen3_base.h"
#include "glen3_storage.h"

ArenaAllocator arena;
#define frame_arena arena

enum TokenType {
    Token_EOF = 1, Token_Int, Token_Ident, Token_Comment, Token_String, Token_Lparen, Token_Rparen,
    Token_Lbracket, Token_Rbracket, Token_Quote
};

const char *format_token_type(TokenType type) {
    switch (type) {
        case Token_EOF: return "EOF";
        case Token_Int: return "<int>";
        case Token_Ident: return "<ident>";
        case Token_Comment: return "<comment>";
        case Token_String: return "<string>";
        case Token_Lparen: return "(";
        case Token_Rparen: return ")";
        case Token_Lbracket: return "[";
        case Token_Rbracket: return "]";
        case Token_Quote: return "'";
    }
    assert(!"Unreachable");
    return "<invalid>";
}

struct SourceLoc {
    const char *srcname;
    s32 line, col;
};

#define LOCFMT(loc) (loc).srcname, (loc).line, (loc).col

struct Token {
    SourceLoc loc;
    TokenType type;
    union {
        s64 i;
        Slice<u8> s;
    };
};

const char *format_token(Token tok) {
    if (tok.type == Token_Ident) {
        return tprint("%.*s", STRFMT(tok.s));
    } else if (tok.type == Token_String) {
        // TODO Escape
        return tprint("\"%.*s\"", STRFMT(tok.s));
    } else if (tok.type == Token_Comment) {
        return tprint("{ %.*s }", STRFMT(tok.s));
    } else {
        return format_token_type(tok.type);
    }
}

bool is_ident_char(u8 ch) {
    return
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= 'a' && ch <= 'z') ||
        ch == '_' || ch == '+' || ch == '-' || ch == '*' || ch == '/' ||
        ch == '<' || ch == '>' || ch == '=';
}

void source_advance(Slice<u8> *src, SourceLoc *loc, s32 n = 1) {
    if (n > src->count) n = src->count;

    for (s32 i = 0; i < n; i++) {
        if ((*src)[0] == '\n') {
            loc->line += 1;
            loc->col = 0;
        }
        loc->col += 1;
        slice_advance(src, 1);
    }
}

Token gettoken(Slice<u8> *src, SourceLoc *loc) {
    while (src->count && isspace((*src)[0]))
        source_advance(src, loc, 1);
    if (!src->count) return {.loc = *loc, .type = Token_EOF, {}};

    if (is_ident_char((*src)[0])) {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Ident;
        tok.s.data = src->data;
        while (src->count && (is_ident_char((*src)[0]) || ((*src)[0] >= '0' && (*src)[0] <= '9'))) {
            tok.s.count += 1;
            source_advance(src, loc, 1);
        }
        return tok;
    } else if ((*src)[0] >= '0' && (*src)[0] <= '9') {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Int;
        while (src->count && ((*src)[0] >= '0' && (*src)[0] <= '9')) {
            tok.i = 10 * tok.i + ((*src)[0] - '0');
            source_advance(src, loc, 1);
        }
        return tok;
    } else if ((*src)[0] == '"') {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_String;
        source_advance(src, loc, 1);
        Array<u8> s = {};
        s.arena = &arena;
        while (src->count && (*src)[0] != '"') {
            array_push(&s, (*src)[0]);
            source_advance(src, loc, 1);
        }
        if ((*src)[0] == '"') source_advance(src, loc, 1);
        tok.s = array_slice(&s);
        return tok;
    } else if ((*src)[0] == '{') {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Comment;
        source_advance(src, loc, 1);
        Array<u8> s = {};
        s.arena = &arena;
        while (src->count && (*src)[0] != '}') {
            array_push(&s, (*src)[0]);
            source_advance(src, loc, 1);
        }
        if ((*src)[0] == '}') source_advance(src, loc, 1);
        tok.s = array_slice(&s);
        trim(&tok.s);
        return tok;
    } else if ((*src)[0] == '(') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Lparen, {}};
    } else if ((*src)[0] == ')') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Rparen, {}};
    } else if ((*src)[0] == '[') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Lbracket, {}};
    } else if ((*src)[0] == ']') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Rbracket, {}};
    } else if ((*src)[0] == '\'') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Quote, {}};
    } else {
        fprintf(stderr, "%s:%d:%d: Unknown start of token '%c' (%d).\n", LOCFMT(*loc), (*src)[0], (*src)[0]);
        exit(1);
    }
}

Slice<Token> tokenize(const char *srcname, Slice<u8> src) {
    Array<Token> tokens = {};
    tokens.arena = &arena;
    SourceLoc loc = {.srcname = srcname, .line = 1, .col = 1};
    while (src.count) {
        Token tok = gettoken(&src, &loc);
        if (tok.type == Token_EOF) break;
        array_push(&tokens, tok);
    }
    return array_slice(&tokens);
}

enum ValueType { Value_Int = 1, Value_String, Value_Comment, Value_Builtin };

struct Value;

struct ValComment {
    SourceLoc loc;
    Value *value;
};

struct ValBuiltin {
    const char *name;
    s32 arity;
    Value (*fn)(Slice<Value> args);
};

struct Value {
    ValueType type;
    union {
        s64 i;
        Slice<u8> s;
        ValBuiltin bf;
    };
};

struct Symbol {
    Slice<u8> name;
    Value value;
};

struct Env {
    Env *super;
    Array<Symbol> symtab;
};

Symbol *bind(Env *env, Slice<u8> name) {
    for (Symbol &sym : env->symtab)
        if (sym.name == name)
            return &sym;

    Symbol *sym = array_push(&env->symtab);
    sym->name = copy_slice_to_arena(&arena, name);
    return sym;
}

Symbol *lookup(Env *env, Slice<u8> name, SourceLoc loc) {
    while (env) {
        for (Symbol &sym : env->symtab)
            if (sym.name == name)
                return &sym;
        env = env->super;
    }

    fprintf(stderr, "%s:%d:%d: Cannot reference unknown name '%.*s'.\n", LOCFMT(loc), STRFMT(name));
    exit(1);
}

const char *format_value(Value val, bool inspect) {
    switch (val.type) {
        case Value_Int: return tprint("%ld", val.i);
        case Value_String: return inspect ? tprint("\"%.*s\"", /* TODO Escape */ STRFMT(val.s)) : tprint("%.*s", STRFMT(val.s));
        case Value_Comment: return tprint("{ %.*s }", STRFMT(val.s));
        case Value_Builtin: return "<builtin>";
    }
    assert(!"Unreachable");
    return "<invalid>";
}

Value b_println(Slice<Value> args) {
    printf("%s\n", format_value(args[0], false));
    return (Value){.type = Value_Int, .i = 1};
}

Value eval(Slice<Token> *tokens, Env *env) {
    if (!tokens->count) {
        fprintf(stderr, "Unexpected end-of-file when trying to evaluate expression.");
        exit(1);
    }

    Token tok = (*tokens)[0];
    if (tok.type == Token_Int) {
        slice_advance(tokens, 1);
        return {.type = Value_Int, .i = tok.i};
    } else if (tok.type == Token_String) {
        slice_advance(tokens, 1);
        return {.type = Value_String, .s = tok.s};
    } else if (tok.type == Token_Ident) {
        slice_advance(tokens, 1);
        Symbol *sym = lookup(env, tok.s, tok.loc);
        if (sym->value.type == Value_Builtin) {
            Array<Value> args = {};
            args.arena = &arena;
            for (s32 i = 0; i < sym->value.bf.arity; i++)
                array_push(&args, eval(tokens, env));
            return sym->value.bf.fn(array_slice(&args));
        } else {
            return sym->value;
        }
    } else {
        fprintf(stderr, "%s:%d:%d: Unexpected start of expression: %s.\n", LOCFMT(tok.loc), format_token(tok));
        exit(1);
    }
}

void usage() {
    fprintf(stderr, "Usage: flamingo FILE\n");
    exit(1);
}

int main(int argc, char **argv) {
    if (argc != 2) usage();
    const char *input = argv[1];

    Slice<u8> src;
    if (!read_entire_file(input, &arena, &src)) {
        fprintf(stderr, "Could not read input file: %s.\n", input);
        exit(1);
    }
    Slice<Token> tokens = tokenize(input, src);

    Env env = {};
    env.symtab.arena = &arena;
    bind(&env, lit_slice("println"))->value = {.type = Value_Builtin, .bf = {.name = "println", .arity = 1, .fn = b_println}};
    while (tokens.count)
        eval(&tokens, &env);

    return 0;
}