#include <stdio.h>
#include <stdlib.h>
#include "glen3_base.h"
#include "glen3_storage.h"

ArenaAllocator arena;
#define frame_arena arena

enum TokenType {
    Token_EOF = 1, Token_Int, Token_Ident, Token_Comment, Token_String, Token_Lparen, Token_Rparen,
    Token_Lbracket, Token_Rbracket, Token_Quote, Token_Line, Token_Lstrlist, Token_Rstrlist,
    Token_KwIf, Token_KwElse, Token_KwEnd, Token_KwFor,
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
        case Token_Line: return "<line>";
        case Token_Lstrlist: return "<<";
        case Token_Rstrlist: return ">>";
        case Token_KwIf: return "if";
        case Token_KwElse: return "else";
        case Token_KwEnd: return "end";
        case Token_KwFor: return "for";
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
    while (src->count && isspace((*src)[0])) {
        if (slice_has_prefix(*src, lit_slice("\n\n"))) {
            SourceLoc l = *loc;
            source_advance(src, loc, 2);
            return {.loc = l, .type = Token_Line, {}};
        }
        source_advance(src, loc, 1);
    }
    if (!src->count) return {.loc = *loc, .type = Token_EOF, {}};

    if (src->count >= 2 && (*src)[0] == '<' && (*src)[0] == '<') { source_advance(src, loc, 2); return {.loc = *loc, .type = Token_Lstrlist, {}}; }
    if (src->count >= 2 && (*src)[0] == '>' && (*src)[0] == '>') { source_advance(src, loc, 2); return {.loc = *loc, .type = Token_Rstrlist, {}}; }

    if (((*src)[0] >= '0' && (*src)[0] <= '9') || ((*src)[0] == '-' && src->count > 1 && (*src)[1] >= '0' && (*src)[1] <= '9')) {
        s64 sign = 1;
        if ((*src)[0] == '-') {
            sign = -1;
            source_advance(src, loc, 1);
        }
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Int;
        while (src->count && ((*src)[0] >= '0' && (*src)[0] <= '9')) {
            tok.i = 10 * tok.i + ((*src)[0] - '0');
            source_advance(src, loc, 1);
        }
        tok.i *= sign;
        return tok;
    } else if (is_ident_char((*src)[0])) {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Ident;
        tok.s.data = src->data;
        while (src->count && (is_ident_char((*src)[0]) || ((*src)[0] >= '0' && (*src)[0] <= '9'))) {
            tok.s.count += 1;
            source_advance(src, loc, 1);
        }
        if (tok.s == lit_slice("if")) tok.type = Token_KwIf;
        if (tok.s == lit_slice("else")) tok.type = Token_KwElse;
        if (tok.s == lit_slice("end")) tok.type = Token_KwEnd;
        if (tok.s == lit_slice("for")) tok.type = Token_KwFor;
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

enum ValueType { Value_Bool = 1, Value_Int, Value_Ident, Value_String, Value_List, Value_Comment, Value_Builtin };

struct Value;
struct Env;

struct ValComment {
    SourceLoc loc;
    Value *value;
};

struct ValBuiltin {
    const char *name;
    s32 arity;
    Value (*fn)(Slice<Value> args, Env *env, SourceLoc loc);
};

struct Value {
    ValueType type;
    union {
        bool b;
        s64 i;
        Slice<u8> s;
        ValComment c;
        ValBuiltin bf;
        Slice<Value> list;
    };
};

bool operator==(const Value &a, const Value &b) {
    if (a.type != b.type) return false;

    switch (a.type) {
        case Value_Bool: return a.b == b.b;
        case Value_Int: return a.i == b.i;
        case Value_Ident: return a.s == b.s;
        case Value_String: return a.s == b.s;
        case Value_Builtin: return a.bf.fn == b.bf.fn;
        case Value_List: {
            if (a.list.count != b.list.count) return false;
            for (s32 i = 0; i < a.list.count; i++)
                if (!(a.list.data[i] == b.list.data[i]))
                    return false;
            return true;
        } break;
        case Value_Comment: {
            if (strcmp(a.c.loc.srcname, b.c.loc.srcname) != 0) return false;
            if (a.c.loc.line != b.c.loc.line) return false;
            if (a.c.loc.col != b.c.loc.col) return false;
            return *a.c.value == *b.c.value;
        } break;
    }

    assert(!"Unreachable");
    return false;
}

FORCEINLINE static inline bool operator!=(const Value &a, const Value &b) {
    return !(a == b);
}

struct Symbol {
    Slice<u8> name;
    Array<Value> assoc;
    Value value;
};

struct Env {
    Env *super;
    Array<Symbol> symtab;
    Value stashed_comment;
};

Symbol *bind(Env *env, Slice<u8> name) {
    for (Symbol &sym : env->symtab)
        if (sym.name == name)
            return &sym;

    Symbol *sym = array_push(&env->symtab);
    sym->name = copy_slice_to_arena(&arena, name);
    sym->assoc.arena = &arena;
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

void assoc(Symbol *sym, Slice<u8> key, Value val) {
    assert((sym->assoc.count & 1) == 0);
    for (s32 i = 0; i < sym->assoc.count; i += 2) {
        if (sym->assoc[i].s == key) {
            sym->assoc[i + 1] = val;
            return;
        }
    }
    array_push(&sym->assoc, {.type = Value_Ident, .s = key});
    array_push(&sym->assoc, val);
}

const char *format_value_type(ValueType type) {
    switch (type) {
        case Value_Bool: return "bool";
        case Value_Int: return "int";
        case Value_Ident: return "ident";
        case Value_String: return "string";
        case Value_List: return "list";
        case Value_Comment: return "comment";
        case Value_Builtin: return "builtin";
    }
    assert(!"Unreachable");
    return "<invalid>";
}

const char *format_value(Value val, bool inspect) {
    switch (val.type) {
        case Value_Bool: return val.b ? "yes" : "no";
        case Value_Int: return tprint("%ld", val.i);
        case Value_Ident: return tprint("%s%.*s", inspect ? "'" : "", STRFMT(val.s));
        case Value_String: return inspect ? tprint("\"%.*s\"", /* TODO Escape */ STRFMT(val.s)) : tprint("%.*s", STRFMT(val.s));
        case Value_Comment: return tprint("{ %s }", format_value(*val.c.value, false));
        case Value_Builtin: return "<builtin>";
        case Value_List: {
            BucketArray<u8> buf = {};
            buf.arena = &arena;
            concat_bytes(&buf, '(');
            for (s32 i = 0; i < val.list.count; i++) {
                if (i > 0) bucket_array_push(&buf, (u8)' ');
                concat_write(&buf, str_slice(format_value(val.list[i], true)));
            }
            concat_bytes(&buf, ')', 0);
            return (char *)bucket_array_linearize(buf, &arena).data;
        } break;
    }
    assert(!"Unreachable");
    return "<invalid>";
}

void stash_comment(Env *env, Value v) {
    assert(v.type == Value_Comment);

    if (!env->stashed_comment.type) {
        env->stashed_comment = v;
        return;
    }
    const char *s = format_value(*env->stashed_comment.c.value, false);
    const char *t = format_value(*v.c.value, false);
    Slice<u8> news = arena_alloc_slice<u8>(&arena, strlen(s) + 1 + strlen(t));
    copy_slice(news.data, str_slice(s));
    news.data[strlen(s)] = '\n';
    copy_slice(news.data + strlen(s) + 1, str_slice(t));
    Value newc = {.type = Value_String, .s = news};
    env->stashed_comment = {.type = Value_Comment, .c = {.loc = env->stashed_comment.c.loc, .value = copy_to_arena(&arena, newc)}};
}


void check_arg(const char *fnname, Slice<Value> args, s32 idx, ValueType required, SourceLoc loc) {
    if (args[idx].type != required) {
        fprintf(stderr, "%s:%d:%d: %s: Argument #%d must be of type %s, not %s.\n", LOCFMT(loc), fnname, idx + 1,
            format_value_type(required), format_value_type(args[idx].type));
        exit(1);
    }
}
#define CHECK_ARG(fnname, idx, required) check_arg(fnname, args, idx, required, loc)

Value b_println(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;

    printf("%s\n", format_value(args[0], false));
    return (Value){.type = Value_Int, .i = 1};
}

Value b_bind(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("bind", 0, Value_Ident);
    Symbol *sym = bind(env, args[0].s);
    sym->assoc.count = 0;
    sym->value = args[1];
    if (env->stashed_comment.type) {
        assoc(sym, lit_slice("doc"), env->stashed_comment);
        env->stashed_comment = {};
    }
    return args[1];
}

Value b_store(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("store", 0, Value_Ident);
    Symbol *sym = bind(env, args[0].s);
    sym->value = args[1];
    return args[1];
}


Value b_assoc(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("assoc", 0, Value_Ident);
    CHECK_ARG("assoc", 1, Value_Ident);
    Symbol *sym = lookup(env, args[0].s, loc);
    assoc(sym, args[1].s, args[2]);
    return args[2];
}

Value b_assoclist(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("assoclist", 0, Value_Ident);
    Symbol *sym = lookup(env, args[0].s, loc);
    return {.type = Value_List, .list = array_slice(&sym->assoc)};
}

Value b_add(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("+", 0, Value_Int);
    CHECK_ARG("+", 1, Value_Int);

    return {.type = Value_Int, .i = args[0].i + args[1].i};
}

Value b_sub(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("-", 0, Value_Int);
    CHECK_ARG("-", 1, Value_Int);

    return {.type = Value_Int, .i = args[0].i - args[1].i};
}

Value b_mul(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("*", 0, Value_Int);
    CHECK_ARG("*", 1, Value_Int);

    return {.type = Value_Int, .i = args[0].i * args[1].i};
}

Value b_div(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("/", 0, Value_Int);
    CHECK_ARG("/", 1, Value_Int);

    return {.type = Value_Int, .i = args[0].i / args[1].i};
}

Value b_mod(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("mod", 0, Value_Int);
    CHECK_ARG("mod", 1, Value_Int);

    if (!args[1].i) {
        fprintf(stderr, "%s:%d:%d: mod: Encountered zero divisor.\n", LOCFMT(loc));
        exit(1);
    }
    return {.type = Value_Int, .i = args[0].i % args[1].i};
}

Value b_eq(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;

    return {.type = Value_Bool, .b = args[0] == args[1]};
}

Value b_ne(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;

    return {.type = Value_Bool, .b = args[0] != args[1]};
}

Value b_lt(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;
    CHECK_ARG("<", 0, Value_Int);
    CHECK_ARG("<", 1, Value_Int);

    return {.type = Value_Bool, .b = args[0].i < args[1].i};
}

Value b_le(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;
    CHECK_ARG("<=", 0, Value_Int);
    CHECK_ARG("<=", 1, Value_Int);

    return {.type = Value_Bool, .b = args[0].i <= args[1].i};
}

Value b_ge(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;
    CHECK_ARG(">=", 0, Value_Int);
    CHECK_ARG(">=", 1, Value_Int);

    return {.type = Value_Bool, .b = args[0].i >= args[1].i};
}

Value b_gt(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;
    CHECK_ARG(">", 0, Value_Int);
    CHECK_ARG(">", 1, Value_Int);

    return {.type = Value_Bool, .b = args[0].i > args[1].i};
}

Value b_tostring(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;

    const char *s = format_value(args[0], false);
    return {.type = Value_String, .s = str_slice(s)};
}

Value b_getloc(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("getloc", 0, Value_Comment);

    Array<Value> list = {};
    list.arena = &arena;
    array_push(&list, {.type = Value_Ident, .s = lit_slice("source")});
    array_push(&list, {.type = Value_String, .s = str_slice(args[0].c.loc.srcname)});
    array_push(&list, {.type = Value_Ident, .s = lit_slice("line")});
    array_push(&list, {.type = Value_Int, .i = args[0].c.loc.line});
    array_push(&list, {.type = Value_Ident, .s = lit_slice("col")});
    array_push(&list, {.type = Value_Int, .i = args[0].c.loc.col});
    return {.type = Value_List, .list = array_slice(&list)};
}

Value b_peel(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("peel", 0, Value_Comment);
    return *args[0].c.value;
}

Value b_make_comment(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("make-comment", 0, Value_String);
    CHECK_ARG("make-comment", 1, Value_Int);
    CHECK_ARG("make-comment", 2, Value_Int);
    ValComment c = {
        .loc = {.srcname = to_c_string(&arena, args[0].s), .line = (s32)args[1].i, .col = (s32)args[2].i},
        .value = copy_to_arena(&arena, args[3])
    };
    return {.type = Value_Comment, .c = c};
}

Value b_stash_comment(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("stash-comment", 0, Value_Comment);
    stash_comment(env, args[0]);
    return args[0];
}

Value b_testtable(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("testtable", 0, Value_Ident);
    CHECK_ARG("testtable", 1, Value_Int);
    CHECK_ARG("testtable", 2, Value_Int);
    CHECK_ARG("testtable", 3, Value_List);

    s32 lower = args[1].i, upper = args[2].i;
    Slice<Value> vals = args[3].list;
    if (vals.count != (upper - lower + 1)) {
        fprintf(stderr, "%s:%d:%d: testtable: Range of integers does not match list size (%d--%d vs %ld).\n", LOCFMT(loc),
            lower, upper, vals.count);
        exit(1);
    }

    for (s32 i = lower; i <= upper; i++) {
        BucketArray<u8> buf = {};
        buf.arena = &arena;
        concat_write(&buf, lit_slice("TESTWITH "));
        concat_write(&buf, args[0].s);
        concat_bytes(&buf, ' ');
        concat_write(&buf, str_slice(tprint("%d", i)));
        concat_bytes(&buf, ' ');
        concat_write(&buf, str_slice(format_value(vals[i], true)));
        concat_bytes(&buf, 0);
        Value v = {.type = Value_String, .s = bucket_array_linearize(buf, &arena)};
        ValComment c = {.loc = loc, .value = copy_to_arena(&arena, v)};
        stash_comment(env, {.type = Value_Comment, .c = c});
    }
    return {.type = Value_Bool, .b = true};
}

Value b_iota(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("iota", 0, Value_Int);

    Slice<Value> list = arena_alloc_slice<Value>(&arena, args[0].i);
    for (s64 i = 0; i < args[0].i; i++)
        list[i] = {.type = Value_Int, .i = i};
    return {.type = Value_List, .list = list};
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
    } else if (tok.type == Token_Comment) {
        slice_advance(tokens, 1);
        Value s = {.type = Value_String, .s = tok.s};
        return {.type = Value_Comment, .c = {.loc = tok.loc, .value = copy_to_arena(&arena, s)}};
    } else if (tok.type == Token_Ident) {
        slice_advance(tokens, 1);
        Symbol *sym = lookup(env, tok.s, tok.loc);
        if (sym->value.type == Value_Builtin) {
            Array<Value> args = {};
            args.arena = &arena;
            for (s32 i = 0; i < sym->value.bf.arity; i++)
                array_push(&args, eval(tokens, env));
            return sym->value.bf.fn(array_slice(&args), env, tok.loc);
        } else {
            return sym->value;
        }
    } else if (tok.type == Token_Quote) {
        slice_advance(tokens, 1);
        if (!tokens->count) {
            fprintf(stderr, "%s:%d:%d: Unexpected end-of-file after '.\n", LOCFMT(tok.loc));
            exit(1);
        }
        Token tok = (*tokens)[0];
        if (tok.type == Token_Ident) {
            slice_advance(tokens, 1);
            return {.type = Value_Ident, .s = tok.s};
        } else {
            fprintf(stderr, "%s:%d:%d: Cannot quote expression beginning with %s.\n", LOCFMT(tok.loc), format_token(tok));
            exit(1);
        }
    } else if (tok.type == Token_Lparen) {
        slice_advance(tokens, 1);
        Array<Value> elems = {};
        elems.arena = &arena;
        while (tokens->count && (*tokens)[0].type != Token_Rparen)
            array_push(&elems, eval(tokens, env));
        if (tokens->count && (*tokens)[0].type == Token_Rparen) slice_advance(tokens, 1);
        return {.type = Value_List, .list = array_slice(&elems)};
    } else if (tok.type == Token_Lstrlist) {
        slice_advance(tokens, 1);
        BucketArray<u8> buf = {};
        buf.arena = &arena;
        while (tokens->count && (*tokens)[0].type != Token_Rstrlist) {
            Value v = eval(tokens, env);
            concat_write(&buf, str_slice(format_value(v, false)));
        }
        if (tokens->count && (*tokens)[0].type == Token_Rstrlist) slice_advance(tokens, 1);
        return {.type = Value_String, .s = bucket_array_linearize(buf, &arena)};
    } else {
        fprintf(stderr, "%s:%d:%d: Unexpected start of expression: %s.\n", LOCFMT(tok.loc), format_token(tok));
        exit(1);
    }
}

bool has_block(TokenType type) {
    return type == Token_KwIf || type == Token_KwFor;
}

Value exec_stmt(Slice<Token> *tokens, Env *env) {
    if (!tokens->count) return {};

    if ((*tokens)[0].type == Token_Line) {
        slice_advance(tokens, 1);
        env->stashed_comment = {};
        return {};
    } else if ((*tokens)[0].type == Token_KwIf) {
        SourceLoc loc = (*tokens)[0].loc;
        slice_advance(tokens, 1);
        Value cond = eval(tokens, env);
        if (cond.type != Value_Bool) {
            fprintf(stderr, "%s:%d:%d: if condition must be of type bool, not %s.\n", LOCFMT(loc), format_value_type(cond.type));
            exit(1);
        }
        Value result = {.type = Value_Bool, .b = false};
        if (cond.b) {
            while (tokens->count && (*tokens)[0].type != Token_KwEnd)
                result = exec_stmt(tokens, env);
            if (tokens->count && (*tokens)[0].type == Token_KwEnd)
                slice_advance(tokens, 1);
        } else {
            s32 balance = 1;
            while (tokens->count && balance > 0) {
                if (has_block((*tokens)[0].type))
                    balance += 1;
                if ((*tokens)[0].type == Token_KwEnd)
                    balance -= 1;
                if ((*tokens)[0].type == Token_KwElse) {
                    slice_advance(tokens, 1);
                    while (tokens->count && (*tokens)[0].type != Token_KwEnd)
                        result = exec_stmt(tokens, env);
                    if (tokens->count && (*tokens)[0].type == Token_KwEnd)
                        slice_advance(tokens, 1);
                    break;
                }
                slice_advance(tokens, 1);
            }
        }
        return result;
    } else if ((*tokens)[0].type == Token_KwFor) {
        SourceLoc loc = (*tokens)[0].loc;
        slice_advance(tokens, 1);
        if (!tokens->count || (*tokens)[0].type != Token_Ident) {
            if (tokens->count)
                fprintf(stderr, "%s:%d:%d: ", LOCFMT((*tokens)[0].loc));
            fprintf(stderr, "Expected identifier in for header, got: %s.\n", tokens->count ? format_token((*tokens)[0]) : "EOF");
            exit(1);
        }
        Slice<u8> var = (*tokens)[0].s;
        slice_advance(tokens, 1);
        Value list = eval(tokens, env);
        if (list.type != Value_List) {
            fprintf(stderr, "%s:%d:%d: for can only iterate over lists, not %s.\n", LOCFMT(loc), format_value(list, true));
            exit(1);
        }
        Slice<Token> block = *tokens;
        if (list.list.count) {
            for (s64 i = 0; i < list.list.count; i++) {
                block = *tokens;
                Env newenv = {};
                newenv.super = env;
                if (var != lit_slice("_")) {
                    newenv.symtab.arena = &arena;
                    bind(&newenv, var)->value = list.list.data[i];
                }
                while (block.count && block[0].type != Token_KwEnd)
                    exec_stmt(&block, &newenv);
            }
            *tokens = block;
        } else {
            s32 balance = 1;
            while (tokens->count && balance > 0) {
                if (has_block((*tokens)[0].type))
                    balance += 1;
                if ((*tokens)[0].type == Token_KwEnd)
                    balance -= 1;
                slice_advance(tokens, 1);
            }
        }
        if (tokens->count && (*tokens)[0].type == Token_KwEnd) slice_advance(tokens, 1);
        return {.type = Value_Bool, .b = true};
    } else {
        SourceLoc loc = (*tokens)[0].loc;
        if (env->stashed_comment.type == Value_Comment &&
            env->stashed_comment.c.value->type == Value_String &&
            slice_has_prefix(env->stashed_comment.c.value->s, lit_slice("TESTWITH ")) &&
            (*tokens)[0].type != Token_Comment)
        {
            Slice<Token> tests = tokenize("<TESTWITH>", env->stashed_comment.c.value->s);
            Slice<Token> stmt;
            Value have;
            while (tests.count) {
                SourceLoc loc = tests[0].loc;
                if (tests[0].type != Token_Ident || tests[0].s != lit_slice("TESTWITH")) {
                    fprintf(stderr, "%s:%d:%d: Unexpected token in TESTWITH directive: %s.\n", LOCFMT(tests[0].loc), format_token(tests[0]));
                    exit(1);
                }
                slice_advance(&tests, 1);
                if (!tests.count || tests[0].type != Token_Ident) {
                    if (tests.count)
                        fprintf(stderr, "%s:%d:%d: ", LOCFMT(tests[0].loc));
                    fprintf(stderr, "Expected identifier in TESTWITH directive, got: %s.\n", tests.count ? format_token(tests[0]) : "EOF");
                    exit(1);
                }
                Slice<u8> var = tests[0].s;
                slice_advance(&tests, 1);
                Value input = eval(&tests, env);
                Value expected = eval(&tests, env);
                stmt = *tokens;
                Env newenv = {};
                newenv.symtab.arena = &arena;
                newenv.super = env;
                bind(&newenv, var)->value = input;
                have = eval(&stmt, &newenv);
                if (have != expected) {
                    fprintf(stderr, "%s:%d:%d: TESTWITH failure: With %.*s=%s the statement produced %s but %s was required.\n",
                        LOCFMT(loc), STRFMT(var), format_value(input, true), format_value(have, true), format_value(expected, true));
                    exit(1);
                }
            }
            *tokens = stmt;
            return have;
        } else {
            Value v = eval(tokens, env);
            if (v.type == Value_Comment) {
                stash_comment(env, v);
            } else if (env->stashed_comment.type == Value_Comment &&
                env->stashed_comment.c.value->type == Value_String &&
                slice_has_prefix(env->stashed_comment.c.value->s, lit_slice("TEST "))
            ) {
                Slice<u8> expr = env->stashed_comment.c.value->s;
                slice_advance(&expr, 5);
                Slice<Token> toks = tokenize("<TEST>", expr);
                Value expected = eval(&toks, env);
                if (v != expected) {
                    fprintf(stderr, "%s:%d:%d: TEST failure: Expression produced %s but %s was required.\n", LOCFMT(loc),
                        format_value(v, true), format_value(expected, true));
                    exit(1);
                }
                env->stashed_comment = {};
            }
            return v;
        }
    }
}

void usage() {
    fprintf(stderr, "Usage: flamingo FILE\n");
    exit(1);
}

int main(int argc, char **argv) {
    Env env = {};
    env.symtab.arena = &arena;
#define BIND_BUILTIN1(nam, fnname, arty) bind(&env, lit_slice(nam))->value = {.type = Value_Builtin, .bf = {.name = nam, .arity = arty, .fn = fnname}}
#define BIND_BUILTIN(nam, arty) BIND_BUILTIN1(#nam, b_##nam, arty)
    BIND_BUILTIN(println, 1);
    BIND_BUILTIN(bind, 2);
    BIND_BUILTIN(store, 2);
    BIND_BUILTIN(assoc, 3);
    BIND_BUILTIN(assoclist, 1);
    BIND_BUILTIN1("+", b_add, 2);
    BIND_BUILTIN1("-", b_sub, 2);
    BIND_BUILTIN1("*", b_mul, 2);
    BIND_BUILTIN1("/", b_div, 2);
    BIND_BUILTIN(mod, 2);
    BIND_BUILTIN1("=", b_eq, 2);
    BIND_BUILTIN1("<>", b_ne, 2);
    BIND_BUILTIN1("<", b_lt, 2);
    BIND_BUILTIN1("<=", b_le, 2);
    BIND_BUILTIN1(">=", b_ge, 2);
    BIND_BUILTIN1(">", b_gt, 2);
    BIND_BUILTIN1("->string", b_tostring, 1);
    BIND_BUILTIN(getloc, 1);
    BIND_BUILTIN(peel, 1);
    BIND_BUILTIN1("make-comment", b_make_comment, 4);
    BIND_BUILTIN1("stash-comment", b_stash_comment, 1);
    BIND_BUILTIN(testtable, 4);
    BIND_BUILTIN(iota, 1);
#undef BIND_BUILTIN
#undef BIND_BUILTIN1
    bind(&env, lit_slice("yes"))->value = {.type = Value_Bool, .b = true};
    bind(&env, lit_slice("no"))->value = {.type = Value_Bool, .b = false};

    if (argc == 1) {
        printf("Entering Flamingo REPL.\nExit by pressing Ctrl-C whenever, Ctrl-D on an empty line, or typing .quit.\n\n");
        for (;;) {
            char line[1024];
            printf("> ");
            fflush(stdout);
            if (!fgets(line, sizeof line, stdin)) break;
            if (line[strlen(line) - 1] == '\n') line[strlen(line) - 1] = 0;
            if (!line[0]) break;
            if (strcmp(line, ".quit") == 0) break;

            Slice<u8> input = copy_slice_to_arena(&arena, str_slice(line));
            Slice<Token> tokens = tokenize("<repl>", input);
            while (tokens.count) {
                Value v = exec_stmt(&tokens, &env);
                if (v.type)
                    printf("%s\n", format_value(v, true));
            }
        }
    } else {
    const char *input = argv[1];
        Slice<u8> src;
        if (!read_entire_file(input, &arena, &src)) {
            fprintf(stderr, "Could not read input file: %s.\n", input);
            exit(1);
        }
        Slice<Token> tokens = tokenize(input, src);

        while (tokens.count)
            exec_stmt(&tokens, &env);
    }

    return 0;
}