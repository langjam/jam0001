#include <stdio.h>
#include <stdlib.h>
#include "glen3_base.h"
#include "glen3_storage.h"
#include <math.h>

ArenaAllocator arena;
#define frame_arena arena

struct Atom {
    s32 inner;
};

bool operator==(Atom a, Atom b) { return a.inner == b.inner; }
bool operator!=(Atom a, Atom b) { return a.inner != b.inner; }

Array<Slice<u8>> intern_pool = {.data = NULL, .count = 0, .capacity = 0, .arena = &arena};

bool switch_uppercase;
bool switch_hex;

Atom intern(Slice<u8> n) {
    if (switch_uppercase) {
        Slice<u8> u = copy_slice_to_arena(&arena, n);
        for (s64 i = 0; i < u.count; i++) u.data[i] = toupper(u.data[i]);
        n = u;
    }

    for (s32 i = 0; i < intern_pool.count; i++)
        if (intern_pool.data[i] == n)
            return {(s32)i};

    array_push(&intern_pool, n);
    return {(s32)intern_pool.count - 1};
}

#define ATOMFMT(a) STRFMT(intern_pool[(a).inner])
Atom atom_doc, atom_len, atom_testwith, atom_source, atom_line, atom_col, atom_arity;

const char *escape_string(Slice<u8> s) {
    BucketArray<u8> buf = {};
    buf.arena = &arena;
    concat_bytes(&buf, '"');
    while (s.count) {
        if (s[0] == '\n') concat_bytes(&buf, '\\', 'n');
        else if (s[0] == '\t') concat_bytes(&buf, '\\', 't');
        else if (s[0] == '"') concat_bytes(&buf, '\\', '"');
        else concat_bytes(&buf, s[0]);
        slice_advance(&s, 1);
    }
    concat_bytes(&buf, '"', 0);
    return (char *)bucket_array_linearize(buf, &arena).data;
}

enum { MAX_ARITY = 16 };

enum TokenType {
    Token_EOF = 1, Token_Int, Token_Float, Token_Ident, Token_ValComment, Token_SwitchComment, Token_String,
    Token_Lparen, Token_Rparen, Token_Lbracket, Token_Rbracket, Token_Quote, Token_Line, Token_Lstrlist,
    Token_Rstrlist, Token_Amp, Token_Comma,
    Token_KwIf, Token_KwElse, Token_KwFor, Token_KwMacro, Token_KwReturn,
};

const char *format_token_type(TokenType type) {
    switch (type) {
        case Token_EOF: return "EOF";
        case Token_Int: return "<int>";
        case Token_Float: return "<float>";
        case Token_Ident: return "<ident>";
        case Token_ValComment: return "<value comment>";
        case Token_SwitchComment: return "<switch comment>";
        case Token_String: return "<string>";
        case Token_Lparen: return "(";
        case Token_Rparen: return ")";
        case Token_Lbracket: return "[";
        case Token_Rbracket: return "]";
        case Token_Quote: return "'";
        case Token_Line: return "<line>";
        case Token_Lstrlist: return "<<";
        case Token_Rstrlist: return ">>";
        case Token_Amp: return "&";
        case Token_Comma: return ",";
        case Token_KwIf: return "if";
        case Token_KwElse: return "else";
        case Token_KwFor: return "for";
        case Token_KwMacro: return "macro";
        case Token_KwReturn: return "return";
    }
    assert(!"Unreachable");
    return "<invalid>";
}

struct SourceLoc {
    const char *srcname;
    s32 line, col;
};

#define LOCFMT(loc) (loc).srcname, (loc).line, (loc).col

struct Error {
    SourceLoc loc;
    const char *msg;
};

struct Token {
    SourceLoc loc;
    TokenType type;
    union {
        s64 i;
        f64 f;
        Slice<u8> s;
        Atom atom;
        u8 swytch;
    };
};

const char *format_token(Token tok) {
    if (tok.type == Token_Int) {
        return tprint("%ld", tok.i);
    } else if (tok.type == Token_Float) {
        return tprint("%g", tok.f);
    } else if (tok.type == Token_Ident) {
        return tprint("%.*s", ATOMFMT(tok.atom));
    } else if (tok.type == Token_String) {
        return escape_string(tok.s);
    } else if (tok.type == Token_ValComment) {
        return tprint("{* %.*s *}", STRFMT(tok.s));
    } else {
        return format_token_type(tok.type);
    }
}

bool is_ident_char(u8 ch) {
    return
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= 'a' && ch <= 'z') ||
        ch == '_' || ch == '+' || ch == '-' || ch == '*' || ch == '/' ||
        ch == '<' || ch == '>' || ch == '=' || ch == '.' || ch == '?' ||
        ch == '!';
}

bool digit2val(u8 ch, s32 *out) {
    s32 val = -1;
    if (switch_hex) {
        if (ch >= '0' && ch <= '9') val = ch - '0';
        if (ch >= 'a' && ch <= 'f') val = 10 + ch - 'a';
        if (ch >= 'A' && ch <= 'F') val = 10 + ch - 'A';
    } else {
        if (ch >= '0' && ch <= '9') val = ch - '0';
    }
    if (out) *out = val;
    return val != -1;
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

Token gettoken(Slice<u8> *src, SourceLoc *loc, Error *err) {
    while (src->count && isspace((*src)[0])) {
        if (slice_has_prefix(*src, lit_slice("\n\n"))) {
            SourceLoc l = *loc;
            source_advance(src, loc, 2);
            return {.loc = l, .type = Token_Line, {}};
        }
        source_advance(src, loc, 1);
    }
    if (!src->count) return {.loc = *loc, .type = Token_EOF, {}};

    if (src->count >= 2 && (*src)[0] == '<' && (*src)[1] == '<') { source_advance(src, loc, 2); return {.loc = *loc, .type = Token_Lstrlist, {}}; }
    if (src->count >= 2 && (*src)[0] == '>' && (*src)[1] == '>') { source_advance(src, loc, 2); return {.loc = *loc, .type = Token_Rstrlist, {}}; }

    if (digit2val((*src)[0], NULL) || ((*src)[0] == '-' && src->count > 1 && digit2val((*src)[1], NULL))) {
        s64 sign = 1;
        if ((*src)[0] == '-') {
            sign = -1;
            source_advance(src, loc, 1);
        }
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Int;
        s32 digit;
        while (src->count && digit2val((*src)[0], &digit)) {
            tok.i = (switch_hex ? 16 : 10) * tok.i + digit;
            source_advance(src, loc, 1);
        }
        if (!src->count || (*src)[0] != '.') {
            tok.i *= sign;
            return tok;
        }
        source_advance(src, loc, 1);

        tok.type = Token_Float;
        s64 dec = 0;
        while (src->count && digit2val((*src)[0], &digit)) {
            dec = (switch_hex ? 16 : 10) * dec + digit;
            source_advance(src, loc, 1);
        }
        f64 fdec = dec;
        while (fdec >= 1) fdec *= (switch_hex ? 1./16 : 0.1);

        tok.f = sign * (tok.i + fdec);
        return tok;
    } else if (is_ident_char((*src)[0])) {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_Ident;
        Slice<u8> ident = {.data = src->data, .count = 0};
        while (src->count && (is_ident_char((*src)[0]) || ((*src)[0] >= '0' && (*src)[0] <= '9'))) {
            ident.count += 1;
            source_advance(src, loc, 1);
        }
        if (ident == lit_slice("if")) tok.type = Token_KwIf;
        if (ident == lit_slice("else")) tok.type = Token_KwElse;
        if (ident == lit_slice("for")) tok.type = Token_KwFor;
        if (ident == lit_slice("macro")) tok.type = Token_KwMacro;
        if (ident == lit_slice("return")) tok.type = Token_KwReturn;
        tok.atom = intern(ident);
        return tok;
    } else if ((*src)[0] == '"') {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_String;
        source_advance(src, loc, 1);
        Array<u8> s = {};
        s.arena = &arena;
        while (src->count && (*src)[0] != '"') {
            if ((*src)[0] == '\\' && src->count > 1) {
                source_advance(src, loc, 1);
                if ((*src)[0] == 'n') array_push(&s, (u8)'\n');
                if ((*src)[0] == 't') array_push(&s, (u8)'\t');
                if ((*src)[0] == '\"') array_push(&s, (u8)'\"');
                source_advance(src, loc, 1);
            } else {
                array_push(&s, (*src)[0]);
                source_advance(src, loc, 1);
            }
        }
        if ((*src)[0] == '"') source_advance(src, loc, 1);
        tok.s = array_slice(&s);
        return tok;
    } else if (slice_has_prefix(*src, lit_slice("{*"))) {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_ValComment;
        source_advance(src, loc, 2);
        tok.s.data = src->data;
        s32 balance = 1;
        while (src->count && balance != 0) {
            if (slice_has_prefix(*src, lit_slice("{*"))) { source_advance(src, loc, 2); balance += 1; }
            else if (slice_has_prefix(*src, lit_slice("*}"))) { source_advance(src, loc, 2); balance -= 1; }
            else if (balance != 0) {
                source_advance(src, loc, 1);
                tok.s.count += 1;
            }
        }
        trim(&tok.s);
        return tok;
    } else if (slice_has_prefix(*src, lit_slice("{-"))) {
        Token tok = {};
        tok.loc = *loc;
        tok.type = Token_SwitchComment;
        source_advance(src, loc, 2);
        while (src->count && isspace((*src)[0]))
            source_advance(src, loc, 1);
        if (!src->count) {
            if (err) { err->loc = *loc; err->msg = "Unexpected end-of-file in switch comment."; }
            return {};
        }
        tok.swytch = (*src)[0];
        if (tolower(tok.swytch) != 't' && tolower(tok.swytch) != 'b' && tolower(tok.swytch) != 'u' && tolower(tok.swytch) != 'h' &&
            tolower(tok.swytch) != 'f' && tolower(tok.swytch) != 'n' ) {
            if (err) { err->loc = *loc; err->msg = tprint("Unknown switch: %c.", tok.swytch); }
            return {};
        }
        source_advance(src, loc, 1);
        while (src->count && isspace((*src)[0]))
            source_advance(src, loc, 1);
        if (slice_has_prefix(*src, lit_slice("-}"))) source_advance(src, loc, 2);
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
    } else if ((*src)[0] == '&') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Amp, {}};
    } else if ((*src)[0] == ',') {
        source_advance(src, loc, 1);
        return {.loc = *loc, .type = Token_Comma, {}};
    } else {
        if (err) { err->loc = *loc; err->msg = tprint("Unknown start of token '%c' (%d).", (*src)[0], (*src)[0]); }
        return {};
    }
}

Slice<Token> tokenize(const char *srcname, Slice<u8> src, Error *err) {
    Array<Token> tokens = {};
    tokens.arena = &arena;
    SourceLoc loc = {.srcname = srcname, .line = 1, .col = 1};
    while (src.count) {
        Token tok = gettoken(&src, &loc, err);
        if (err->msg) break;
        if (tok.type == Token_EOF) break;
        if (tok.type == Token_SwitchComment) {
            if (tolower(tok.swytch) == 'u') switch_uppercase = tok.swytch == 'U';
            if (tolower(tok.swytch) == 'h') switch_hex = tok.swytch == 'H';
            else array_push(&tokens, tok);
            continue;
        }
        array_push(&tokens, tok);
    }
    return array_slice(&tokens);
}

enum ValueType {
    Value_Bool = 1, Value_Int, Value_Float, Value_Ident, Value_String, Value_List, Value_ValueComment, Value_SwitchComment,
    Value_Builtin, Value_Block, Value_Macro, Value_Return, Value_Failure,
};

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

struct ValMacro {
    s64 param_count;
    Slice<Token> body;
};

struct Value {
    ValueType type;
    union {
        bool b;
        s64 i;
        f64 f;
        Slice<u8> s;
        Atom atom;
        ValComment c;
        ValBuiltin bf;
        Slice<Value> list;
        Slice<Token> body;
        ValMacro macro;
        Value *ret;
        u8 swytch;
        Error fail;
    };
};

bool operator==(const Value &a, const Value &b) {
    if (a.type != b.type) return false;

    switch (a.type) {
        case Value_Bool: return a.b == b.b;
        case Value_Int: return a.i == b.i;
        case Value_Float: return a.f == b.f;
        case Value_Ident: return a.atom == b.atom;
        case Value_String: return a.s == b.s;
        case Value_Builtin: return a.bf.fn == b.bf.fn;
        case Value_List: {
            if (a.list.count != b.list.count) return false;
            for (s32 i = 0; i < a.list.count; i++)
                if (!(a.list.data[i] == b.list.data[i]))
                    return false;
            return true;
        } break;
        case Value_ValueComment: {
            if (strcmp(a.c.loc.srcname, b.c.loc.srcname) != 0) return false;
            if (a.c.loc.line != b.c.loc.line) return false;
            if (a.c.loc.col != b.c.loc.col) return false;
            return *a.c.value == *b.c.value;
        } break;
        case Value_SwitchComment: return a.swytch == b.swytch;
        case Value_Block: return false;
        case Value_Macro: return false;
        case Value_Return: assert(!"Unreachable"); return false;
        case Value_Failure: assert(!"Unreachable"); return false;
    }

    assert(!"Unreachable");
    return false;
}

FORCEINLINE static inline bool operator!=(const Value &a, const Value &b) {
    return !(a == b);
}

Value fail(bool failhard, SourceLoc loc, const char *fmt, ...) {
    if (failhard) {
        fprintf(stderr, "%s:%d:%d: ", LOCFMT(loc));
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
        exit(1);
    }

    va_list args;
    va_start(args, fmt);
    s32 size = vsnprintf(NULL, 0, fmt, args);
    va_end(args);
    char *msg = arena_alloc_many<char>(&arena, size + 1);
    va_start(args, fmt);
    vsprintf(msg, fmt, args);
    va_end(args);
    msg[size] = 0;
    return {.type = Value_Failure, .fail = {.loc = loc, .msg = msg}};
}

struct Symbol {
    Atom name;
    Value value;
    Array<Value> assoc;
};

Symbol failure_sym;

struct EnvPage {
    Symbol syms[32];
    s32 count;
    EnvPage *next;
};

struct Env {
    Env *super;
    EnvPage *first_page;
    Slice<Value> params;
    Value stashed_comment;
    bool switch_testing;
    bool switch_stepping;
    bool switch_failhard;
    bool switch_narrating;
};

Env new_environment(Env *super) {
    return {
        .super = super, .first_page = NULL, .params = {}, .stashed_comment = {},
        .switch_testing = super ? super->switch_testing : true,
        .switch_stepping = super ? super->switch_stepping : false,
        .switch_failhard = super ? super->switch_failhard : true,
        .switch_narrating = super ? super->switch_narrating : false,
    };
}

void free_environment(Env *env) {
    EnvPage *cur = env->first_page;
    while (cur) {
        EnvPage *next = cur->next;
        free(cur);
        cur = next;
    }
}

Symbol *bind(Env *env, Atom name) {
    for (EnvPage *cur = env->first_page; cur; cur = cur->next)
        for (s32 i = 0; i < cur->count; i++)
            if (cur->syms[i].name == name)
                return cur->syms + i;

    EnvPage **page = &env->first_page;
    while (*page) {
        if ((*page)->count < ARRAY_COUNT(EnvPage::syms)) break;
        page = &(*page)->next;
    }
    if (!*page) *page = (EnvPage *)calloc(1, sizeof **page);
    assert((*page)->count < ARRAY_COUNT(EnvPage::syms));
    Symbol *sym = (*page)->syms + (*page)->count++;
    sym->name = name;
    sym->assoc.arena = &arena;
    return sym;
}

Symbol *try_lookup(Env *env, Atom name) {
    while (env) {
        for (EnvPage *cur = env->first_page; cur; cur = cur->next)
            for (s32 i = 0; i < cur->count; i++)
                if (cur->syms[i].name == name)
                    return cur->syms + i;
        env = env->super;
    }
    return NULL;
}

Symbol *lookup(Env *env, Atom name, SourceLoc loc) {
    Symbol *sym = try_lookup(env, name);
    if (sym) return sym;

    if (env->switch_failhard) {
        fprintf(stderr, "%s:%d:%d: Cannot reference unknown name '%.*s'.\n", LOCFMT(loc), ATOMFMT(name));
        exit(1);
    } else {
        failure_sym.name = name;
        failure_sym.value = {.type = Value_Failure, .fail = {.loc = loc, .msg = tprint("Cannot reference unknown name: '%.*s'.", ATOMFMT(name))}};
        return &failure_sym;
    }
}

void assoc(Symbol *sym, Atom key, Value val) {
    assert((sym->assoc.count & 1) == 0);
    for (s32 i = 0; i < sym->assoc.count; i += 2) {
        if (sym->assoc[i].atom == key) {
            sym->assoc[i + 1] = val;
            return;
        }
    }
    array_push(&sym->assoc, {.type = Value_Ident, .atom = key});
    array_push(&sym->assoc, val);
}

bool getassoc(Symbol *sym, Atom key, Value *out) {
    assert((sym->assoc.count & 1) == 0);
    for (s32 i = 0; i < sym->assoc.count; i += 2) {
        if (sym->assoc[i].atom == key) {
            if (out) *out = sym->assoc[i + 1];
            return true;
        }
    }
    return false;
}

void set_switch(Env *env, u8 swytch) {
    if (tolower(swytch) == 't') env->switch_testing = swytch == 'T';
    if (tolower(swytch) == 'b') env->switch_stepping = swytch == 'B';
    if (tolower(swytch) == 'f') env->switch_failhard = swytch == 'F';
    if (tolower(swytch) == 'n') env->switch_narrating = swytch == 'N';
}

bool get_switch(Env *env, u8 swytch) {
    if (tolower(swytch) == 't') return env->switch_testing;
    if (tolower(swytch) == 'b') return env->switch_stepping;
    if (tolower(swytch) == 'f') return env->switch_failhard;
    if (tolower(swytch) == 'n') return env->switch_narrating;
    assert(!"Unreachable");
    return false;
}

const char *format_value_type(ValueType type) {
    switch (type) {
        case Value_Bool: return "bool";
        case Value_Int: return "int";
        case Value_Float: return "float";
        case Value_Ident: return "ident";
        case Value_String: return "string";
        case Value_List: return "list";
        case Value_ValueComment: return "value comment";
        case Value_SwitchComment: return "switch comment";
        case Value_Builtin: return "builtin";
        case Value_Block: return "block";
        case Value_Macro: return "macro";
        case Value_Return: assert(!"Unreachable"); return "<invalid>";
        case Value_Failure: assert(!"Unreachable"); return "<invalid>";
    }
    assert(!"Unreachable");
    return "<invalid>";
}

const char *format_value(Value val, bool inspect) {
    switch (val.type) {
        case Value_Bool: return val.b ? "yes" : "no";
        case Value_Int: return tprint("%ld", val.i);
        case Value_Float: return tprint("%g", val.f);
        case Value_Ident: return tprint("%s%.*s", inspect ? "'" : "", ATOMFMT(val.atom));
        case Value_String: return inspect ? escape_string(val.s) : tprint("%.*s", STRFMT(val.s));
        case Value_ValueComment: return tprint("{* %s *}", format_value(*val.c.value, false));
        case Value_SwitchComment: return tprint("{- %c *}", val.swytch);
        case Value_Builtin: return "<builtin>";
        case Value_List: {
            BucketArray<u8> buf = {};
            buf.arena = &arena;
            concat_bytes(&buf, '(');
            for (s32 i = 0; i < val.list.count; i++) {
                if (i > 0) concat_bytes(&buf, ' ');
                concat_write(&buf, str_slice(format_value(val.list[i], true)));
            }
            concat_bytes(&buf, ')', 0);
            return (char *)bucket_array_linearize(buf, &arena).data;
        } break;
        case Value_Block: {
            BucketArray<u8> buf = {};
            buf.arena = &arena;
            concat_bytes(&buf, '[', ' ');
            for (s64 i = 0; i < val.body.count; i++) {
                if (i > 0) concat_bytes(&buf, ' ');
                concat_write(&buf, str_slice(format_token(val.body.data[i])));
            }
            concat_bytes(&buf, ']', 0);
            return (char *)bucket_array_linearize(buf, &arena).data;
        } break;
        case Value_Macro: return tprint("<macro/%ld +%dl>", val.macro.param_count, val.macro.body.count);
        case Value_Return: assert(!"Unreachable"); break;
        case Value_Failure: assert(!"Unreachable"); break;
    }
    assert(!"Unreachable");
    return "<invalid>";
}

void stash_comment(Env *env, Value v) {
    assert(v.type == Value_ValueComment);

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
    env->stashed_comment = {.type = Value_ValueComment, .c = {.loc = env->stashed_comment.c.loc, .value = copy_to_arena(&arena, newc)}};
}

#define CHECK_ARG(fnname, idx, required) \
    do { \
        if (args[idx].type != (required)) \
            return fail(env->switch_failhard, loc, "%s: Argument #%d must be of type %s, not %s.", (fnname), (idx) + 1, \
                format_value_type(required), format_value_type(args[idx].type)); \
    } while (0)

Value b_println(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;

    printf("%s\n", format_value(args[0], false));
    return (Value){.type = Value_Int, .i = 1};
}

Value b_bind(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("bind", 0, Value_Ident);
    Symbol *sym = bind(env, args[0].atom);
    sym->assoc.count = 0;
    sym->value = args[1];
    if (env->stashed_comment.type) {
        assoc(sym, atom_doc, env->stashed_comment);
        env->stashed_comment = {};
    }
    return args[1];
}

Value b_store(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("store", 0, Value_Ident);
    Symbol *sym = try_lookup(env, args[0].atom);
    if (!sym) sym = bind(env, args[0].atom);
    sym->value = args[1];
    return args[1];
}


Value b_assoc(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("assoc", 0, Value_Ident);
    CHECK_ARG("assoc", 1, Value_Ident);
    Symbol *sym = lookup(env, args[0].atom, loc);
    assoc(sym, args[1].atom, args[2]);
    return args[2];
}

Value b_assoclist(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("assoclist", 0, Value_Ident);
    Symbol *sym = lookup(env, args[0].atom, loc);
    return {.type = Value_List, .list = array_slice(&sym->assoc)};
}

Value b_add(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;

    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Int, .i = args[0].i + args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Float, .f = fa + fb};
    } else if (args[0].type == Value_Block && args[1].type == Value_Block) {
        Array<Token> body = array_alloc<Token>(&arena, args[0].body.count + args[1].body.count);
        for (Token &tok : args[0].body) array_push(&body, tok);
        for (Token &tok : args[1].body) array_push(&body, tok);
        return {.type = Value_Block, .body = array_slice(&body)};
    } else if (args[0].type == Value_List && args[1].type == Value_List) {
        Array<Value> list = array_alloc<Value>(&arena, args[0].list.count + args[0].list.count);
        for (Value &val : args[0].list) array_push(&list, val);
        for (Value &val : args[1].list) array_push(&list, val);
        return {.type = Value_List, .list = array_slice(&list)};
    } else {
        return fail(env->switch_failhard, loc, "+: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_sub(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Int, .i = args[0].i - args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Float, .f = fa - fb};
    } else {
        return fail(env->switch_failhard, loc, "-: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_mul(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Int, .i = args[0].i * args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Float, .f = fa * fb};
    } else {
        return fail(env->switch_failhard, loc, "*: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_div(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Int, .i = args[0].i / args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Float, .f = fa / fb};
    } else {
        return fail(env->switch_failhard, loc, "/: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_fdiv(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;

    if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Float, .f = fa / fb};
    } else {
        return fail(env->switch_failhard, loc, "/.: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_mod(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Int, .i = args[0].i % args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Float, .f = fmod(fa, fb)};
    } else {
        return fail(env->switch_failhard, loc, "mod: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
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
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Bool, .b = args[0].i < args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Bool, .b = fa < fb};
    } else {
        return fail(env->switch_failhard, loc, "<: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_le(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Bool, .b = args[0].i <= args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Bool, .b = fa <= fb};
    } else {
        return fail(env->switch_failhard, loc, "<=: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_ge(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Bool, .b = args[0].i >= args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Bool, .b = fa >= fb};
    } else {
        return fail(env->switch_failhard, loc, ">=: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_gt(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Int && args[1].type == Value_Int) {
        return {.type = Value_Bool, .b = args[0].i > args[1].i};
    } else if ((args[0].type == Value_Int || args[0].type == Value_Float) && (args[1].type == Value_Int || args[1].type == Value_Float)) {
        f64 fa = args[0].type == Value_Int ? args[0].i : args[0].f;
        f64 fb = args[1].type == Value_Int ? args[1].i : args[1].f;
        return {.type = Value_Bool, .b = fa > fb};
    } else {
        return fail(env->switch_failhard, loc, ">: Incompatible argument types %s and %s.",
            format_value_type(args[0].type), format_value_type(args[1].type));
    }
}

Value b_and(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("and", 0, Value_Bool);
    CHECK_ARG("and", 1, Value_Bool);

    return {.type = Value_Bool, .b = args[0].b && args[1].b};
}

Value b_or(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("or", 0, Value_Bool);
    CHECK_ARG("or", 1, Value_Bool);

    return {.type = Value_Bool, .b = args[0].b || args[1].b};
}

Value b_not(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    if (args[0].type == Value_Bool) {
        return {.type = Value_Bool, .b = !args[0].b};
    } else if (args[0].type == Value_SwitchComment) {
        u8 s = args[0].swytch == tolower(args[0].swytch) ? toupper(args[0].swytch) : tolower(args[0].swytch);
        return {.type = Value_SwitchComment, .swytch = s};
    } else {
        return fail(env->switch_failhard, loc, "not: Incompatible argument type: %s.", format_value_type(args[0].type));
    }
}

Value b_tostring(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;

    const char *s = format_value(args[0], false);
    return {.type = Value_String, .s = str_slice(s)};
}

Value b_getloc(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("getloc", 0, Value_ValueComment);

    Array<Value> list = {};
    list.arena = &arena;
    array_push(&list, {.type = Value_Ident, .atom = atom_source});
    array_push(&list, {.type = Value_String, .s = str_slice(args[0].c.loc.srcname)});
    array_push(&list, {.type = Value_Ident, .atom = atom_line});
    array_push(&list, {.type = Value_Int, .i = args[0].c.loc.line});
    array_push(&list, {.type = Value_Ident, .atom = atom_col});
    array_push(&list, {.type = Value_Int, .i = args[0].c.loc.col});
    return {.type = Value_List, .list = array_slice(&list)};
}

Value b_peel(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("peel", 0, Value_ValueComment);
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
    return {.type = Value_ValueComment, .c = c};
}

Value b_stash_comment(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("stash-comment", 0, Value_ValueComment);
    stash_comment(env, args[0]);
    return args[0];
}

Value b_switch_p(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("switch?", 0, Value_SwitchComment);
    return {.type = Value_Bool, .b = args[0].swytch == toupper(args[0].swytch)};
}

Value b_switch_do(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("switch!", 0, Value_SwitchComment);
    set_switch(env, args[0].swytch);
    return {.type = Value_Bool, .b = get_switch(env, args[0].swytch)};
}

Value b_env_switch_p(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("env-switch?", 0, Value_SwitchComment);
    return {.type = Value_Bool, .b = get_switch(env, args[0].swytch)};
}

Value b_testtable(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("testtable", 0, Value_Ident);
    CHECK_ARG("testtable", 1, Value_List);
    CHECK_ARG("testtable", 2, Value_List);

    Slice<Value> ins = args[1].list, wants = args[2].list;

    if (ins.count != wants.count) {
        return fail(env->switch_failhard, loc, "testtable: Input and expected list must match in size; encountered %ld and %ld.",
            ins.count, wants.count);
    }

    for (s64 i = 0; i < ins.count; i++) {
        BucketArray<u8> buf = {};
        buf.arena = &arena;
        concat_write(&buf, lit_slice("TESTWITH "));
        concat_write(&buf, intern_pool[args[0].atom.inner]);
        concat_bytes(&buf, ' ');
        concat_write(&buf, str_slice(format_value(ins[i], true)));
        concat_bytes(&buf, ' ');
        concat_write(&buf, str_slice(format_value(wants[i], true)));
        concat_bytes(&buf, 0);
        Value v = {.type = Value_String, .s = bucket_array_linearize(buf, &arena)};
        ValComment c = {.loc = loc, .value = copy_to_arena(&arena, v)};
        stash_comment(env, {.type = Value_ValueComment, .c = c});
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

Value b_iota_plus(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("iota+", 0, Value_Int);
    CHECK_ARG("iota+", 1, Value_Int);

    Slice<Value> list = arena_alloc_slice<Value>(&arena, args[1].i - args[0].i);
    for (s64 i = args[0].i; i < args[1].i; i++)
        list[i - args[0].i] = {.type = Value_Int, .i = i};
    return {.type = Value_List, .list = list};
}

Value exec_block(Slice<Token> tokens, Env *env);

Value b_eval(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("eval", 0, Value_Block);
    Value result = exec_block(args[0].body, env);
    if (result.type == Value_Return) result = *result.ret;
    return result;
}

Value b_apply(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("apply", 0, Value_Block);
    CHECK_ARG("apply", 1, Value_List);
    Env newenv = new_environment(env);
    newenv.params = args[1].list;
    Value result = exec_block(args[0].body, &newenv);
    if (result.type == Value_Return) result = *result.ret;
    free_environment(&newenv);
    return result;
}

Value b_recover(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("eval", 0, Value_Block);
    Value result = exec_block(args[0].body, env);
    if (result.type == Value_Return) result = *result.ret;
    if (result.type == Value_Failure) {
        Value s = {.type = Value_String, .s = str_slice(result.fail.msg)};
        result = {.type = Value_ValueComment, .c = {.loc = result.fail.loc, .value = copy_to_arena(&arena, s)}};
    }
    return result;
}

Value b_getparam(Slice<Value> args, Env *env, SourceLoc loc) {
    CHECK_ARG("getparam", 0, Value_Int);
    if (args[0].i < 0 || args[0].i >= env->params.count)
        return fail(env->switch_failhard, loc, "Requested parameter #%ld is out-of-bounds for current environment.", args[0].i + 1);
    return env->params[args[0].i];
}

Value b_floor(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("floor", 0, Value_Float);

    return {.type = Value_Float, .f = floor(args[0].f)};
}

Value b_sqrt(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("sqrt", 0, Value_Float);

    return {.type = Value_Float, .f = sqrt(args[0].f)};
}

Value b_sin(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("sin", 0, Value_Float);

    return {.type = Value_Float, .f = sin(args[0].f)};
}

Value b_cos(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("cos", 0, Value_Float);

    return {.type = Value_Float, .f = cos(args[0].f)};
}

Value b_float2int(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("float->int", 0, Value_Float);

    return {.type = Value_Int, .i = (s64)args[0].f};
}

Value b_type(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env; (void)loc;
    Atom atom = {};
    switch (args[0].type) {
        case Value_Bool: atom = intern(lit_slice("bool")); break;
        case Value_Int: atom = intern(lit_slice("int")); break;
        case Value_Float: atom = intern(lit_slice("float")); break;
        case Value_Ident: atom = intern(lit_slice("ident")); break;
        case Value_String: atom = intern(lit_slice("string")); break;
        case Value_ValueComment: atom = intern(lit_slice("value-comment")); break;
        case Value_SwitchComment: atom = intern(lit_slice("switch-comment")); break;
        case Value_Builtin: atom = intern(lit_slice("builtin")); break;
        case Value_List: atom = intern(lit_slice("list")); break;
        case Value_Block: atom = intern(lit_slice("block")); break;
        case Value_Macro: atom = intern(lit_slice("macro")); break;
        case Value_Return: assert(!"Unreachable"); break;
        case Value_Failure: assert(!"Unreachable"); break;
    }
    return {.type = Value_Ident, .atom = atom};
}

Value b_len(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("len", 0, Value_List);
    return {.type = Value_Int, .i = args[0].list.count};
}

Value b_at(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("at", 0, Value_List);
    CHECK_ARG("at", 1, Value_Int);
    if (args[1].i < 0 || args[1].i >= args[0].list.count)
        return fail(env->switch_failhard, loc, "Index out of range: %ld for list of length %ld.", args[1].i, args[0].list.count);
    return args[0].list[args[1].i];
}

Value b_error(Slice<Value> args, Env *env, SourceLoc loc) {
    (void)env;
    CHECK_ARG("error", 0, Value_String);
    return {.type = Value_Failure, .fail = {.loc = loc, .msg = tprint("%.*s", STRFMT(args[0].s))}};
}

Slice<Token> get_macro_arg(Slice<Token> *tokens) {
    if (!tokens->count) return {};

    if ((*tokens)[0].type == Token_Lparen || (*tokens)[0].type == Token_Lbracket) {
        slice_advance(tokens, 1);
        Slice<Token> result = {.data = tokens->data, .count = 0};
        s32 balance = 1;
        while (tokens->count) {
            if ((*tokens)[0].type == Token_Lparen || (*tokens)[0].type == Token_Lbracket) balance += 1;
            if ((*tokens)[0].type == Token_Rparen || (*tokens)[0].type == Token_Rbracket) balance -= 1;
            slice_advance(tokens, 1);
            if (balance == 0) break;
            result.count += 1;
        }
        return result;
    } else {
        Slice<Token> result = {.data = tokens->data, .count = 1};
        slice_advance(tokens, 1);
        return result;
    }
}

Error get_macro_param_ref(s64 *out, Slice<Token> *tokens, s64 params_cnt, const char *ctx, SourceLoc loc) {
    if (!tokens->count) return {.loc = loc, .msg = tprint("Macro %s expects argument.", ctx)};

    Token tok = (*tokens)[0];
    if (tok.type != Token_Int) return {.loc = tok.loc, .msg = tprint("Macro %s expects argument reference, not %s.", ctx, format_token(tok))};
    if (tok.i < 0 || tok.i >= params_cnt) return {.loc = tok.loc, .msg = tprint("Macro argument #%ld is out of bounds. %ld arguments are available.", tok.i, params_cnt)};

    slice_advance(tokens, 1);
    if (out) *out = tok.i;
    return {};
}

Error eval_macro(Slice<Token> *sout, Slice<Token> tokens, Slice<Slice<Token>> params) {
    Array<Token> out = {};
    out.arena = &arena;
    while (tokens.count) {
        if (tokens[0].type != Token_Comma) {
            array_push(&out, tokens[0]);
            slice_advance(&tokens, 1);
            continue;
        }
        SourceLoc loc = tokens[0].loc;
        slice_advance(&tokens, 1);
        if (!tokens.count) {
            fprintf(stderr, "%s:%d:%d: Expected token after , in macro evaluation.\n", LOCFMT(loc));
            exit(1);
        }

        if (tokens[0].type == Token_Int) {
            s64 arg;
            Error err = get_macro_param_ref(&arg, &tokens, params.count, "", loc);
            if (err.msg) return err;
            for (Token &tok : params[arg])
                array_push(&out, tok);
        } else if (tokens[0].type == Token_Ident && tokens[0].atom == atom_len) {
            slice_advance(&tokens, 1);
            s64 arg;
            Error err = get_macro_param_ref(&arg, &tokens, params.count, "len", loc);
            if (err.msg) return err;
            array_push(&out, {.loc = loc, .type = Token_Int, .i = params[arg].count});
        } else if (tokens[0].type == Token_KwFor) {
            slice_advance(&tokens, 1);
            s64 arg;
            Error err = get_macro_param_ref(&arg, &tokens, params.count, "for", loc);
            if (err.msg) return err;
            Slice<Token> body = get_macro_arg(&tokens);
            Slice<Slice<Token>> newparams = arena_alloc_slice<Slice<Token>>(&arena, params.count + 2);
            copy_slice(newparams.data, params);
            for (s64 i = 0; i < params[arg].count; i++) {
                newparams[params.count] = {.data = &params[arg][i], .count = 1};
                Token tok_i = {.loc = loc, .type = Token_Int, .i = i};
                newparams[params.count+1] = {.data = &tok_i, .count = 1};
                Slice<Token> result;
                Error err = eval_macro(&result, body, newparams);
                if (err.msg) return err;
                for (Token &tok : result)
                    array_push(&out, tok);
            }
        }
    }
    if (sout) *sout = array_slice(&out);
    return {};
}

Value eval(Slice<Token> *tokens, Env *env) {
    if (!tokens->count) return {.type = Value_Bool, .b = false};

    Token tok = (*tokens)[0];
    if (tok.type == Token_Int) {
        slice_advance(tokens, 1);
        return {.type = Value_Int, .i = tok.i};
    } else if (tok.type == Token_Float) {
        slice_advance(tokens, 1);
        return {.type = Value_Float, .f = tok.f};
    } else if (tok.type == Token_String) {
        slice_advance(tokens, 1);
        return {.type = Value_String, .s = tok.s};
    } else if (tok.type == Token_ValComment) {
        slice_advance(tokens, 1);
        Value s = {.type = Value_String, .s = tok.s};
        return {.type = Value_ValueComment, .c = {.loc = tok.loc, .value = copy_to_arena(&arena, s)}};
    } else if (tok.type == Token_SwitchComment) {
        slice_advance(tokens, 1);
        return {.type = Value_SwitchComment, .swytch = tok.swytch};
    } else if (tok.type == Token_Ident) {
        slice_advance(tokens, 1);
        Symbol *sym = lookup(env, tok.atom, tok.loc);
        if (sym->value.type == Value_Builtin) {
            Value args[16];
            for (s32 i = 0; i < sym->value.bf.arity; i++)
                args[i] = eval(tokens, env);
            return sym->value.bf.fn({.data = args, .count = sym->value.bf.arity}, env, tok.loc);
        } else if (sym->value.type == Value_Block) {
            Value arity;
            if (getassoc(sym, atom_arity, &arity)) {
                if (arity.type != Value_Int)
                    return fail(env->switch_failhard, tok.loc, "Associated arity for %.*s is not an integer but %s.", ATOMFMT(sym->name), format_value_type(arity.type));
                if (arity.i > MAX_ARITY)
                    return fail(env->switch_failhard, tok.loc, "%.*s exceeds maximum arity of %d arguments.", ATOMFMT(sym->name), MAX_ARITY);
                Value args[MAX_ARITY];
                for (s64 i = 0; i < arity.i; i++)
                    args[i] =  eval(tokens, env);
                Env newenv = new_environment(env);
                newenv.params = {.data = args, .count = arity.i};
                Value result = exec_block(sym->value.body, &newenv);
                if (result.type == Value_Return) result = *result.ret;
                free_environment(&newenv);
                return result;
            } else {
                return sym->value;
            }
        } else if (sym->value.type == Value_Macro) {
            Array<Slice<Token>> args = {};
            args.arena = &arena;
            for (s64 i = 0; i < sym->value.macro.param_count; i++)
                array_push(&args, get_macro_arg(tokens));
            Slice<Token> body;
            Error err = eval_macro(&body, sym->value.macro.body, array_slice(&args));
            if (err.msg) return {.type = Value_Failure, .fail = err};
            return exec_block(body, env);
        } else {
            return sym->value;
        }
    } else if (tok.type == Token_Quote) {
        slice_advance(tokens, 1);
        if (!tokens->count)
            fail(env->switch_failhard, tok.loc, "Unexpected end-of-file after '.");
        Token tok = (*tokens)[0];
        if (tok.type == Token_Ident) {
            slice_advance(tokens, 1);
            return {.type = Value_Ident, .atom = tok.atom};
        } else {
            return fail(env->switch_failhard, tok.loc, "Cannot quote expression beginning with %s.", format_token(tok));
        }
    } else if (tok.type == Token_Amp) {
        slice_advance(tokens, 1);
        if (!tokens->count)
            return fail(env->switch_failhard, tok.loc, "Unexpected end-of-file after &.");
        Token tok = (*tokens)[0];
        if (tok.type == Token_Ident) {
            slice_advance(tokens, 1);
            return lookup(env, tok.atom, tok.loc)->value;
        } else {
            return fail(env->switch_failhard, tok.loc, "An identifier is required for function quoting, not %s.", format_token(tok));
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
    } else if (tok.type == Token_Lbracket) {
        slice_advance(tokens, 1);
        Slice<Token> body = {.data = tokens->data, .count = 0};
        s32 balance = 1;
        while (tokens->count) {
            if ((*tokens)[0].type == Token_Lbracket) balance += 1;
            if ((*tokens)[0].type == Token_Rbracket) balance -= 1;
            slice_advance(tokens, 1);
            if (balance == 0) break;
            body.count += 1;
        }
        return {.type = Value_Block, .body = body};
    } else {
        return fail(env->switch_failhard, tok.loc, "Unexpected start of expression: %s.", format_token(tok));
    }
}

void debugger_repl(Slice<Token> *tokens, Env *env) {
    for (;;) {
        fprintf(stderr, "%s:%d:%d> ", LOCFMT((*tokens)[0].loc));
        fflush(stderr);
        char buf[1024];
        if (!fgets(buf, sizeof buf, stdin)) {
            env->switch_stepping = false;
            break;
        }
        char *p = buf;

        while (*p && isspace(*p)) p += 1;
        Slice<u8> cmd = {.data = (u8 *)p, .count = 0};
        while (*p && !isspace(*p)) {
            cmd.count += 1;
            p += 1;
        }

        while (*p && isspace(*p)) p += 1;
        Slice<u8> arg1 = {.data = (u8 *)p, .count = 0};
        while (*p && !isspace(*p)) {
            arg1.count += 1;
            p += 1;
        }

        while (*p && isspace(*p)) p += 1;
        Slice<u8> arg2 = str_slice(p);

        if (cmd == lit_slice("n") || cmd == lit_slice("next")) {
            break;
        } else if (cmd == lit_slice("cont") || cmd == lit_slice("continue")) {
            env->switch_stepping = false;
            break;
        } else if (cmd == lit_slice("exit")) {
            exit(0);
        } else if (cmd == lit_slice("peek")) {
            s32 n = 0;
            for (u8 ch : arg1) {
                if (!isdigit(ch)) break;
                n = 10 * n + (ch - '0');
            }
            if (n > tokens->count) n = tokens->count;
            for (s32 i = 0; i < n; i++)
                fprintf(stderr, "%s\n", format_token((*tokens)[i]));
        } else if (cmd == lit_slice("skip")) {
            s32 n = 0;
            for (u8 ch : arg1) {
                if (!isdigit(ch)) break;
                n = 10 * n + (ch - '0');
            }
            slice_advance(tokens, n);
        } else if (cmd == lit_slice("p") || cmd == lit_slice("print")) {
            Error err = {};
            Slice<Token> toks = tokenize("<debug>", arg1, &err);
            if (!err.msg) {
                Value result = eval(&toks, env);
                if (result.type != Value_Failure)
                    fprintf(stderr, "%s\n", format_value(result, true));
            }
        } else if (cmd == lit_slice("set")) {
            Error err = {};
            Slice<Token> toks = tokenize("<debug>", arg2, &err);
            if (!err.msg) {
                Value result = eval(&toks, env);
                if (result.type == Value_Failure) {
                    fprintf(stderr, "An error occured while evaluating the expression:\n");
                    fprintf(stderr, "%s:%d:%d: %s\n", LOCFMT(result.fail.loc), result.fail.msg);
                } else {
                    fprintf(stderr, "Binding %.*s to %s.\n", STRFMT(arg1), format_value(result, true));
                    bind(env, intern(arg1))->value = result;
                }
            }
        }
    }
}

Value exec_stmt(Slice<Token> *tokens, Env *env) {
    if (!tokens->count) return {};

    if (env->switch_stepping) debugger_repl(tokens, env);

    if (env->switch_narrating && env->stashed_comment.type == Value_ValueComment)
        fprintf(stderr, "%s:%d:%d: %s\n", LOCFMT(env->stashed_comment.c.loc), format_value(*env->stashed_comment.c.value, false));

    if ((*tokens)[0].type == Token_Line) {
        slice_advance(tokens, 1);
        env->stashed_comment = {};
        return {.type = Value_Bool, .b = false};
    } else if ((*tokens)[0].type == Token_KwIf) {
        SourceLoc loc = (*tokens)[0].loc;
        slice_advance(tokens, 1);
        Value cond = eval(tokens, env);
        if (cond.type != Value_Bool)
            return fail(env->switch_failhard, loc, "if condition must be of type bool, not %s.", format_value_type(cond.type));
        Value iftrue = eval(tokens, env);
        if (iftrue.type != Value_Block)
            return fail(env->switch_failhard, loc, "if body must be of type block, not %s.", format_value_type(iftrue.type));
        Value iffalse = {};
        if (tokens->count && (*tokens)[0].type == Token_KwElse) {
            slice_advance(tokens, 1);
            iffalse = eval(tokens, env);
            if (iffalse.type != Value_Block)
                return fail(env->switch_failhard, loc, "else body must be of type block, not %s.\n", format_value_type(iffalse.type));
        }
        Value result = {.type = Value_Bool, .b = false};
        if (cond.b)
            result = exec_block(iftrue.body, env);
        else if (iffalse.type)
            result = exec_block(iffalse.body, env);
        return result;
    } else if ((*tokens)[0].type == Token_KwFor) {
        SourceLoc loc = (*tokens)[0].loc;
        slice_advance(tokens, 1);
        if (!tokens->count || (*tokens)[0].type != Token_Ident)
            return fail(env->switch_failhard, loc, "Expected identifier in for header, got: %s.", tokens->count ? format_token((*tokens)[0]) : "EOF");
        Atom var = (*tokens)[0].atom;
        slice_advance(tokens, 1);
        Value list = eval(tokens, env);
        if (list.type != Value_List)
            return fail(env->switch_failhard, loc, "for can only iterate over lists, not %s.", format_value(list, true));
        Value body = eval(tokens, env);
        if (body.type != Value_Block)
            return fail(env->switch_failhard, loc, "for body must be of type block, not %s.", format_value_type(body.type));
        Value result = {.type = Value_Bool, .b = false};
        for (s64 i = 0; i < list.list.count; i++) {
            Env newenv = new_environment(env);
            if (intern_pool[var.inner] != lit_slice("_"))
                bind(&newenv, var)->value = list.list.data[i];
            result = exec_block(body.body, &newenv);
            if (result.type == Value_Return) return result;
            free_environment(&newenv);
        }
        return {.type = Value_Bool, .b = true};
    } else if ((*tokens)[0].type == Token_KwMacro) {
        SourceLoc loc = (*tokens)[0].loc;
        slice_advance(tokens, 1);
        if (!tokens->count)
            return fail(env->switch_failhard, loc, "Macro definition requires a name.");
        if ((*tokens)[0].type != Token_Ident)
            return fail(env->switch_failhard, loc, "Macro name must be an identifier, not %s.", format_token((*tokens)[0]));
        Atom name = (*tokens)[0].atom;
        slice_advance(tokens, 1);
        if (!tokens->count)
            return fail(env->switch_failhard, loc, "Macro definition requires parameter count.");
        if ((*tokens)[0].type != Token_Int)
            return fail(env->switch_failhard, loc, "Macro name must be an identifier, not %s.", format_token((*tokens)[0]));
        s64 param_count = (*tokens)[0].i;
        slice_advance(tokens, 1);
        Slice<Token> body = get_macro_arg(tokens);
        ValMacro macro = {.param_count = param_count, .body = body};
        Value val = {.type = Value_Macro, .macro = macro};
        bind(env, name)->value = val;
        return val;
    } else if ((*tokens)[0].type == Token_KwReturn) {
        slice_advance(tokens, 1);
        Value result = {.type = Value_Return, .ret = NULL};
        if (tokens->count)
            result.ret = copy_to_arena(&arena, eval(tokens, env));
        else
            result.ret = copy_to_arena(&arena, (Value){.type = Value_Bool, .b = false});
        return result;
    } else {
        SourceLoc loc = (*tokens)[0].loc;
        if (env->switch_testing &&
            env->stashed_comment.type == Value_ValueComment &&
            env->stashed_comment.c.value->type == Value_String &&
            slice_has_prefix(env->stashed_comment.c.value->s, lit_slice("TESTWITH ")) &&
            (*tokens)[0].type != Token_ValComment)
        {
            Error err = {};
            Slice<Token> tests = tokenize("<TESTWITH>", env->stashed_comment.c.value->s, &err);
            if (err.msg) return fail(env->switch_failhard, err.loc, "TESTWITH: %s.", err.msg);
            Slice<Token> stmt;
            Value have;
            while (tests.count) {
                SourceLoc loc = tests[0].loc;
                if (tests[0].type != Token_Ident || tests[0].atom != atom_testwith)
                    return fail(env->switch_failhard, tests[0].loc, "Unexpected token in TESTWITH directive: %s.", format_token(tests[0]));
                slice_advance(&tests, 1);
                if (!tests.count || tests[0].type != Token_Ident)
                    return fail(env->switch_failhard, loc, "Expected identifier in TESTWITH directive, got: %s.", tests.count ? format_token(tests[0]) : "EOF");
                Atom var = tests[0].atom;
                slice_advance(&tests, 1);
                Value input = eval(&tests, env);
                Value expected = eval(&tests, env);
                stmt = *tokens;
                Env newenv = new_environment(env);
                bind(&newenv, var)->value = input;
                have = eval(&stmt, &newenv);
                if (have != expected)
                    return fail(env->switch_failhard, loc, "TESTWITH failure: With %.*s=%s the statement produced %s but %s was required.",
                        ATOMFMT(var), format_value(input, true), format_value(have, true), format_value(expected, true));
                free_environment(&newenv);
            }
            *tokens = stmt;
            return have;
        } else {
            Value v = eval(tokens, env);
            if (v.type == Value_ValueComment) {
                stash_comment(env, v);
            } else if (v.type == Value_SwitchComment) {
                set_switch(env, v.swytch);
            } else if (env->switch_testing &&
                env->stashed_comment.type == Value_ValueComment &&
                env->stashed_comment.c.value->type == Value_String &&
                slice_has_prefix(env->stashed_comment.c.value->s, lit_slice("TEST "))
            ) {
                Slice<u8> expr = env->stashed_comment.c.value->s;
                slice_advance(&expr, 5);
                Error err = {};
                Slice<Token> toks = tokenize("<TEST>", expr, &err);
                if (err.msg) return fail(env->switch_failhard, loc, "TEST: %s.", err.msg);
                Value expected = eval(&toks, env);
                if (v != expected)
                    return fail(env->switch_failhard, loc, "TEST failure: Expression produced %s but %s was required.\n",
                        format_value(v, true), format_value(expected, true));
                env->stashed_comment = {};
            }
            return v;
        }
    }
}

Value exec_block(Slice<Token> tokens, Env *env) {
    Value result = {.type = Value_Bool, .b = false};
    while (tokens.count) {
        result = exec_stmt(&tokens, env);
        if (result.type == Value_Return) break;
    }
    return result;
}

int main(int argc, char **argv) {
    if (argc == 2 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
        fprintf(stderr, "Usage: flamingo [-U] [FILE]\n");
        fprintf(stderr, "Call flamingo without an argument to start the REPL.\n");
        fprintf(stderr, "Call it with a file argument to execute that file.\n");
        fprintf(stderr, "-U: Enable uppercase normalization of identifiers.\n");
        exit(1);
    }

    const char *input = NULL;
    for (s32 i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-U") == 0) {
            switch_uppercase = true;
        } else {
            input = argv[i];
        }
    }

    intern(lit_slice("</>")); // Make Atom(0) invalid.
    atom_doc = intern(lit_slice("doc"));
    atom_len = intern(lit_slice("len"));
    atom_testwith = intern(lit_slice("TESTWITH"));
    atom_source = intern(lit_slice("source"));
    atom_line = intern(lit_slice("line"));
    atom_col = intern(lit_slice("col"));
    atom_arity = intern(lit_slice("arity"));

    Env env = new_environment(NULL);
#define BIND_BUILTIN1(nam, fnname, arty) \
    do { \
        assert((arty) < MAX_ARITY); \
        Value v = {.type = Value_Builtin, .bf = {.name = (nam), .arity = arty, .fn = fnname}}; \
        bind(&env, intern(lit_slice(nam)))->value = v; \
    } while (0)
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
    BIND_BUILTIN1("/.", b_fdiv, 2);
    BIND_BUILTIN(mod, 2);
    BIND_BUILTIN1("=", b_eq, 2);
    BIND_BUILTIN1("<>", b_ne, 2);
    BIND_BUILTIN1("<", b_lt, 2);
    BIND_BUILTIN1("<=", b_le, 2);
    BIND_BUILTIN1(">=", b_ge, 2);
    BIND_BUILTIN1(">", b_gt, 2);
    BIND_BUILTIN(and, 2);
    BIND_BUILTIN(or, 2);
    BIND_BUILTIN(not, 1);
    BIND_BUILTIN1("->string", b_tostring, 1);
    BIND_BUILTIN(getloc, 1);
    BIND_BUILTIN(peel, 1);
    BIND_BUILTIN1("make-comment", b_make_comment, 4);
    BIND_BUILTIN1("stash-comment", b_stash_comment, 1);
    BIND_BUILTIN1("switch?", b_switch_p, 1);
    BIND_BUILTIN1("switch!", b_switch_do, 1);
    BIND_BUILTIN1("env-switch?", b_env_switch_p, 1);
    BIND_BUILTIN(testtable, 3);
    BIND_BUILTIN(iota, 1);
    BIND_BUILTIN1("iota+", b_iota_plus, 2);
    BIND_BUILTIN(eval, 1);
    BIND_BUILTIN(apply, 2);
    BIND_BUILTIN(recover, 1);
    BIND_BUILTIN(getparam, 1);
    BIND_BUILTIN(floor, 1);
    BIND_BUILTIN(sqrt, 1);
    BIND_BUILTIN(sin, 1);
    BIND_BUILTIN(cos, 1);
    BIND_BUILTIN1("float->int", b_float2int, 1);
    BIND_BUILTIN(type, 1);
    BIND_BUILTIN(len, 1);
    BIND_BUILTIN(at, 2);
    BIND_BUILTIN(error, 1);
#undef BIND_BUILTIN
#undef BIND_BUILTIN1
    bind(&env, intern(lit_slice("yes")))->value = {.type = Value_Bool, .b = true};
    bind(&env, intern(lit_slice("no")))->value = {.type = Value_Bool, .b = false};
    {
        Error err = {};
        const char s[] = R"EOT(
            bind ',0 [ ,2 ]
            assoc ',0 'arity ,len 1
            ,for 1 [
                store ',0 + [ bind ',3 getparam ,4 ] &,0
            ]
        )EOT";
        Slice<Token> body = tokenize("<defun macro>", lit_slice(s), &err);
        assert(!err.msg);
        bind(&env, intern(lit_slice("defun")))->value = {.type = Value_Macro, .macro = {.param_count = 3, .body = body}};
    }
    {
        Error err = {};
        Slice<Token> body = tokenize("<debug macro>", lit_slice("println << ,for 0 [ ',1 \": \" ,1 \"  \" ] >>"), &err);
        assert(!err.msg);
        bind(&env, intern(lit_slice("debug")))->value = {.type = Value_Macro, .macro = {.param_count = 1, .body = body}};
    }


    if (!input) {
        printf("Entering Flamingo REPL.\nExit by pressing Ctrl-C whenever, Ctrl-D on an empty line, or typing .quit.\n\n");
        env.switch_failhard = false;
        for (;;) {
            char line[1024];
            printf("> ");
            fflush(stdout);
            if (!fgets(line, sizeof line, stdin)) break;
            if (line[strlen(line) - 1] == '\n') line[strlen(line) - 1] = 0;
            if (!line[0]) break;
            if (strcmp(line, ".quit") == 0) break;

            Slice<u8> input = copy_slice_to_arena(&arena, str_slice(line));
            Error err = {};
            Slice<Token> tokens = tokenize("<repl>", input, &err);
            if (err.msg) {
                fprintf(stderr, "%s:%d:%d: %s\n", LOCFMT(err.loc), err.msg);
            } else {
                Value v = exec_block(tokens, &env);
                if (v.type == Value_Failure) {
                    fprintf(stderr, "%s:%d:%d: %s\n", LOCFMT(v.fail.loc), v.fail.msg);
                } else {
                    printf("%s\n", format_value(v, true));
                }
            }
        }
    } else {
        Slice<u8> src;
        if (!read_entire_file(input, &arena, &src)) {
            fprintf(stderr, "Could not read input file: %s.\n", input);
            exit(1);
        }
        Error err = {};
        Slice<Token> tokens = tokenize(input, src, &err);
        if (err.msg) {
            fprintf(stderr, "%s:%d:%d: %s\n", LOCFMT(err.loc), err.msg);
            return 1;
        }

        exec_block(tokens, &env);
    }

    return 0;
}
