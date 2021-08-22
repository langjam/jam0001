#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
typedef struct Parser_Node pnode_t;
typedef struct Parser_State pstate_t;
typedef struct Token tok_t;
typedef enum Parser_Node_Kind pnode_kind_t;

static struct Parser_State parser;

struct {
    rune unmatched;
    string expected;
    usize delimpos;
    bool fail, panicking;
} error_handling = {
    .expected = NULL,
    .unmatched = 0,
    .fail = false,
    .panicking = false
};

static usize pos() {
    return parser.current_token.span.from;
}   

struct Vec OF(strview_t) preceding_comments;

static void add_comments() {
    // Nocom
    struct Span spn = lex_get_comment(&parser.lexer);
    while (!(spn.from == 0 && spn.size == 0)) {
        strview_t view = strview_span(spn, parser.lexer.src);
        vec_push(&preceding_comments, &view);
        spn = lex_get_comment(&parser.lexer);
    }
}

static struct Vec leak_comments() {
    struct Vec old = preceding_comments;
    preceding_comments = vec_new(sizeof(strview_t));
    return old;
}

static rune begindelim(rune delim) {
    rune prev = error_handling.unmatched;
    error_handling.unmatched = delim;
    return prev;
}

static void lineinfo(tok_t *token) {
    if (error_handling.expected != NULL) {
        EH_MESSAGE(". %s", error_handling.expected);
    }
    eh_error(token->line, token->col, parser.lexer.src);
}

static pnode_t mkinval() {
    return (pnode_t) { .kind = PN_INVAL };
}
static tok_t eof(tok_t tok);

static tok_t pull() {
    tok_t tok = parser.current_token;
    parser.current_token = lex_determine(&parser.lexer);
    if (tok.tt == TT_INVALID) {
        EH_MESSAGE("Invalid character: `%.*s`", (int) parser.current_token.span.size, parser.lexer.src+parser.current_token.span.from);
        lineinfo(&tok);
        exit(1);
    }
    if (tok.tt == TT_EOF) {
        eof(tok);
    }
    return tok;
}

static pnode_t inval() {
    error_handling.panicking = true;
    EH_MESSAGE("Invalid expression");
    lineinfo(&parser.current_token);
    pull();
    error_handling.fail = true;
    return mkinval();
}
static bool isinval(pnode_t node) {
    return node.kind == PN_INVAL;
}

static void setexpect(string expect) {
    error_handling.expected = expect;
}

static void enddelim(rune prev) {
    error_handling.unmatched = prev;
}

__attribute__((unused))
static void ptok(tok_t tok) {
    printf("%s | %.*s\n", TT_NAMES[tok.tt], (int)tok.span.size, tok.span.from+parser.lexer.src);
}

static tok_t eof(tok_t tok) {
    EH_MESSAGE("Unexpected end of file");
    if (error_handling.unmatched != 0) {
        EH_MESSAGE(", enclosed in `%c`", error_handling.unmatched);
    }
    struct Token faketoken = {
        .col = parser.lexer.col,
        .line = parser.lexer.line,
    };
    lineinfo(&faketoken);
    if (parser.lexer.src[tok.span.from] == '\"') {
        fprintf(stderr, "(Probably due to unclosed \")\n");
        eh_at_line(tok.line, parser.lexer.src);
        eh_point(tok.col);
    }
    exit(1);
}



static tok_t peek() {
    return parser.current_token;
}

static bool skipdelim(enum Token_Type tt) {
    if (peek().tt == tt) {
        if (tt == TT_COMMA)
            EH_MESSAGE("Extraneous comma."); 
        if (tt == TT_SEMI)
            EH_MESSAGE("Extraneous semicolon."); 
        error_handling.fail = true;
        tok_t t = peek();
        lineinfo(&t);
        while (peek().tt == tt) pull();   
        return true;
    }
    return false;
}

bool check(const string against) {
    return spanstreqstr(parser.current_token.span, parser.lexer.src, against);
}

static void stray(tok_t *tok) {
    if (tok->tt == TT_EOF)
        eof(*tok);
    EH_MESSAGE("Stray `%.*s`", SPAN_PF(tok->span, parser.lexer.src));
    lineinfo(tok);
    exit(1);
}

static void assert_tt(tok_t *tok, enum Token_Type tt) {
    if (tok->tt != tt) {
        stray(tok);
    }
}

static bool skip_tt(enum Token_Type tt) {
    if (error_handling.panicking) return true;
    tok_t tok = pull();
    if (tok.tt != tt) {
        stray(&tok);
    }
    return false;
}

__attribute__((unused))
static void skip_v(const string v) {
    if (error_handling.panicking) return;
    tok_t tok = pull();
    if (!spanstreqstr(tok.span, parser.lexer.src, v)) {
        stray(&tok);
    }
}

static void pnode_checkfree(pnode_t *node) {
    if (!isinval(*node)) {
        for (usize i = 0; i < node->children.size; i += 1)
            pnode_checkfree(vec_get(&node->children, i));
        vec_drop(&node->children);
        *node = mkinval();
    }
}

void pnode_attach(pnode_t *left, pnode_t right) {
    if (isinval(right)) {
        pnode_checkfree(left);
    }
    if (isinval(*left))
        return;

    vec_push(&left->children, &right);
}

pnode_t pnode_listing(usize pos, pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .pos = pos,
        .addressing = PA_LISTING,
        .children = vec_new(sizeof(pnode_t))
    };
}

pnode_t pnode_unary(usize pos, pnode_kind_t kind, pnode_t left) {
    pnode_t node = {
        .kind = kind,
        .pos = pos,
        .addressing = PA_UNARY,
        .children = vec_new(sizeof(pnode_t))
    };
    pnode_attach(&node, left);
    return node;
}

pnode_t pnode_binary(usize pos, pnode_kind_t kind, pnode_t left, pnode_t right) {
    pnode_t node = {
        .kind = kind,
        .pos = pos,
        .addressing = PA_BINARY,
        .children = vec_new(sizeof(pnode_t))
    };
    pnode_attach(&node, left);
    pnode_attach(&node, right);
    return node;
}

pnode_t pnode_ternary(usize pos, pnode_kind_t kind, pnode_t cond, pnode_t left, pnode_t right) {
    pnode_t node = {
        .kind = kind,
        .pos = pos,
        .addressing = PA_TERNARY,
        .children = vec_new(sizeof(pnode_t))
    };
    pnode_attach(&node, cond);
    pnode_attach(&node, left);
    pnode_attach(&node, right);
    return node;
}

pnode_t *pnode_uvalue(pnode_t *of) {
    if (of->addressing != PA_UNARY) {
        fprintf(stderr, "Internal error: Invalid addressing mode for unary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 0);
}

pnode_t *pnode_cond(pnode_t *of) {
    if (of->addressing != PA_TERNARY) {
        fprintf(stderr, "Internal error: Invalid addressing mode for ternary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 0);
}

pnode_t *pnode_body(pnode_t *of) {
    if (of->addressing != PA_TERNARY) {
        fprintf(stderr, "Internal error: Invalid addressing mode for ternary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 1);
}

pnode_t *pnode_alt(pnode_t *of) {
    if (of->addressing != PA_TERNARY) {
        fprintf(stderr, "Internal error: Invalid addressing mode for ternary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 2);
}

pnode_t *pnode_left(pnode_t *of) {
    if (of->addressing != PA_BINARY) {
        fprintf(stderr, "Internal error: Invalid addressing mode for binary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 0);
}

pnode_t *pnode_right(pnode_t *of) {
    if (!(of->addressing == PA_BINARY)) {
        fprintf(stderr, "Internal error: Invalid addressing mode for binary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 1);
}

static pnode_t pnode_endpoint(usize pos, pnode_kind_t kind) {
    return (pnode_t) {
        .pos = pos,
        .kind = kind,
        .addressing = PA_ENDPOINT,
    };
}

static pnode_t delimited(pnode_kind_t kind, const string open, enum Token_Type between, const string shut, bool mustclose, bool autohandle, pnode_t callback()) {
    if (kind == PN_TYPELIST) 
        setexpect("Did you mean to create a parameter list?");
    pnode_t node = pnode_listing(pos(), kind);
    skip_v(open);
    setexpect(NULL);
    rune delim = begindelim(*open);
    while (!check(shut)) {
        add_comments();
        pnode_t v = callback();
        pnode_attach(&node, v);
        if (error_handling.panicking) {
            usize depth = 0;
            while (true) {
                if (depth == 0 && check(shut)) break;
                if (peek().tt == between) break;
                if (check(open)) depth += 1;
                if (check(shut)) depth -= 1;
                pull();
            }
            error_handling.panicking = false;
        }
        if (check(shut) && (!mustclose)) break;
        else if (!autohandle)
            if (!check(shut) || mustclose) { 
                if (between == TT_COMMA)
                    setexpect("Expected comma instead");
                else if (between == TT_SEMI)
                    setexpect("Expected semicolon instead");
                skip_tt(between);
                setexpect(NULL);
            }
        skipdelim(between);
    }
    setexpect(NULL);
    enddelim(delim);
    skip_v(shut);
    return node;
}


static pnode_t most_important_expression();
static pnode_t value();
static pnode_t maybe_call() {
    usize start = pos();
    pnode_t left = value();
    if (check("("))
        return pnode_binary(start, PN_CALL, left, delimited(PN_PARAMS, "(", TT_COMMA, ")", false, false, most_important_expression));
    else if (check("{")) {
        skip_tt(TT_LBRACE);
        ptok(peek());
        pnode_t node = pnode_binary(start, PN_ACCESS, left, most_important_expression());
        skip_tt(TT_RBRACE);
        return node;
    }
    else if (check("'")) {
        pull();
        tok_t name = pull();
        assert_tt(&name, TT_IDENT);
        pnode_t node = pnode_unary(start, PN_FIELD, left);
        node.data.field.name = strview_span(name.span, parser.lexer.src);
        return node;
    }
    return left;
}

static pnode_t declaration(tok_t on);

static struct Parser_Type typedecl() {
    struct Vec OF(usize) depths = vec_new(sizeof(usize));
    while (peek().tt == TT_INTEGER || peek().tt == TT_DEF) {
        usize siz = 0;
        if (peek().tt == TT_INTEGER) {
            tok_t t = pull();
            siz = (usize)strtol((const char *)(t.span.from+parser.lexer.src), NULL, 10);
        }
        vec_push(&depths, &siz);
        skip_tt(TT_DEF);
    }
    setexpect("Expected type name");
    tok_t token = pull();
    assert_tt(&token, TT_IDENT);
    setexpect(NULL);
    return (struct Parser_Type) {
        .name = strview_span(token.span, parser.lexer.src),
        .depths = depths
    };
}

#define OPS_W 4
#define OPS_H 11

static const string OPERATORS[OPS_H][OPS_W] = {
    { "*", "/", "%" },
    { "+", "-" },
    { "<<", ">>" },
    { "<", ">", ">=", "<=" },
    { "==", "!=" },
    { "&" },
    { "^" },
    { "|" },
    { "&&" },
    { "||" },
    { "=" },
};

usize getprec(struct Span op) {
    for (usize i = 0; i < OPS_H; i += 1) {
        for (usize j = 0; j < OPS_W; j += 1) {
            if (OPERATORS[i][j] == NULL) continue;
            if (spanstreqstr(op, parser.lexer.src, OPERATORS[i][j])) return i+1;
        }
    }
    return 0;
}

static pnode_t parse_expr(usize prec) {
    if (prec == 0) return maybe_call();
    pnode_t left = parse_expr(prec-1);
    while ((peek().tt == TT_OPERATOR || peek().tt == TT_PIPE) && getprec(peek().span) == prec) {
        pnode_kind_t kind = PN_OPERATOR;
        struct Span op = peek().span;
        usize start = pos();
        skip_tt(TT_OPERATOR);
        if (spanstreqstr(op, parser.lexer.src, "="))
            kind = PN_ASSIGN;
        pnode_t right = parse_expr(prec-1);
        left = pnode_binary(start, kind, left, right);
        left.data.op.op = strview_span(op, parser.lexer.src); 
    }
    return left;
}

static pnode_t most_important_expression() {
    return parse_expr(OPS_H);
}

static pnode_t pulldeclaration() {
    return declaration(pull());
}

static pnode_t statement();

static pnode_t body() {
    return delimited(PN_BODY, "{", TT_SEMI, "}", true, true, statement);
}

static pnode_t statement() {
    usize start = pos();
    if (check("{")) {
        return body();
    }
    else if (check("while")) {
        skip_v("while");
        rune delim = begindelim('(');
        setexpect("(While requires condition wrapped in parentheses)");
        skip_v("(");
        pnode_t left = most_important_expression();
        skip_v(")");
        setexpect(NULL);
        enddelim(delim);
        pnode_t node = pnode_binary(start, PN_WHILE, left, statement());
        return node;
    }
    else if (check("return")) {
        skip_v("return");
        pnode_t node = pnode_unary(start, PN_RETURN, most_important_expression());
        skip_tt(TT_SEMI);
        return node;
    }
    else if (check("if")) {
        skip_v("if");
        rune delim = begindelim('(');
        setexpect("(Ifs require condition wrapped in parentheses)");
        skip_v("(");
        pnode_t cond = most_important_expression();
        skip_v(")");
        setexpect(NULL);
        enddelim(delim);
        pnode_t body = statement();
        if (check("else")) {
            skip_v("else");
            pnode_t n = pnode_ternary(start, PN_IF, cond, body, statement());
            return n;
        }
        else {
            return pnode_binary(start, PN_IF, cond, body);
        }
    }
    pnode_t node = most_important_expression();
    setexpect("Expected semicolon instead");
    skip_tt(TT_SEMI);
    setexpect(NULL);
    return node;
}

static pnode_t initializer() {
    usize start = pos();
    tok_t field = pull();
    assert_tt(&field, TT_IDENT);
    skip_tt(TT_DEF);
    pnode_t node = pnode_unary(start, PN_INIT, most_important_expression());
    node.data.init.name = strview_span(field.span, parser.lexer.src);
    return node;
}

static pnode_t atom() {
    usize start = pos();
    pnode_t value_node;
    enum Parser_Number_Kind kind;
    tok_t token = peek();
    switch (token.tt) {
        case TT_STRING:
            pull();
            value_node = pnode_endpoint(start, PN_STRING);
            value_node.data.string.val = strview_span(token.span, parser.lexer.src);
            return value_node;
        case TT_HEXADECIMAL:
            kind = PNM_HEX; goto meat;
        case TT_BINARY:
            kind = PNM_BIN; goto meat;
        case TT_OCTAL:
            kind = PNM_OCT; goto meat;
        case TT_INTEGER:
            kind = PNM_INT; goto meat;
        case TT_FLOATING:
            kind = PNM_FLT; goto meat;
        meat:
            pull();
            value_node = pnode_endpoint(start, PN_NUMBER);
            value_node.data.number.val = strview_span(token.span, parser.lexer.src);
            value_node.data.number.kind = kind;
            return value_node;
        case TT_IDENT: {
            pull();
            value_node = pnode_endpoint(start, PN_IDENT);
            value_node.data.ident.val = strview_span(token.span, parser.lexer.src);
            return value_node;
        }
        default:
            return inval();
    }
}

static pnode_t macro() {
    usize start = pos();
    setexpect("Macros are limited to only basic features, refer to documentation");
    pnode_t node = pnode_listing(start, PN_COMMAND);
    while (!(check("|") || check("]"))) {
        pnode_t n = atom();
        if (isinval(n)) break;
        pnode_attach(&node, n);
    }
    setexpect(NULL);
    return node;
}

static pnode_t value() {
    usize start = pos();
    tok_t token = peek();
    pnode_t value_node;
    switch (token.tt) {
        case TT_NEW:{ 
            pull();
            struct Parser_Type type = typedecl();
            value_node = delimited(PN_NEW, "{", TT_COMMA, "}", false, false, initializer);
            value_node.data.newinst.type = type;
            return value_node;
        }
        case TT_STRUCT:
            pull();
            return delimited(PN_STRUCT, "{", TT_COMMA, "}", false, false, pulldeclaration);
        case TT_PROC:
            pull();
            pnode_t left = delimited(PN_TYPELIST, "(", TT_COMMA, ")", false, false, pulldeclaration);
            struct Parser_Type returntype = typedecl();
            value_node = pnode_binary(
                start,
                PN_PROC, 
                left,
                body()
            );
            value_node.data.proc.return_type = returntype;
            return value_node;
        case TT_LBRACKET:
            return delimited(PN_MACRO, "[", TT_PIPE, "]", false, false, macro);
        case TT_IDENT: {
            pull();
            if (peek().tt == TT_DEF) {
                return declaration(token);
            }
            else {
                value_node = pnode_endpoint(start, PN_IDENT);
                value_node.data.ident.val = strview_span(token.span, parser.lexer.src);
                return value_node;
            }
        }
        case TT_LPAREN:
            if (skip_tt(TT_LPAREN)) return inval();
            value_node = most_important_expression();
            skip_tt(TT_RPAREN);
            return value_node;
        case TT_LBRACE:
            value_node = body();
            return value_node;
        case TT_RPAREN:
        case TT_RBRACKET:
        case TT_RBRACE:
            stray(&token);
        case TT_OPERATOR:
            if (
                spanstreqstr(token.span, parser.lexer.src, "!") ||
                spanstreqstr(token.span, parser.lexer.src, "-")
            ) {
                pull();
                value_node = pnode_unary(start, PN_UNARY, value());
                value_node.data.unary.op = strview_span(token.span, parser.lexer.src);
                return value_node;
            }
        default: 
            return atom();
    }
}

static pnode_t declaration(tok_t on) {
    usize start = pos();
    assert_tt(&on, TT_IDENT);
    setexpect("Expected `:` followed by type");
    skip_tt(TT_DEF);
    setexpect(NULL);
    pnode_t decl_node = pnode_endpoint(start, PN_DECL);
    decl_node.data.decl.name = strview_span(on.span, parser.lexer.src);
    decl_node.data.decl.type = (struct Parser_Type) {
        .name = strview_from("_"),
        .depths = vec_new(sizeof(usize))
    };
    if (!check("=")) {
        vec_drop(&decl_node.data.decl.type.depths);
        decl_node.data.decl.type = typedecl();
    }
    decl_node.data.decl.annotations = leak_comments();
    return decl_node;
}


void parser_init(const string src) {
    preceding_comments = vec_new(sizeof(strview_t));
    parser = (struct Parser_State) {
        .lexer = lex_new(src)
    };
    add_comments();
    parser.current_token = lex_determine(&parser.lexer);
}

struct Parser_State *parser_get_state() {
    return &parser;
}

void parser_deinit() {
    lex_drop(&parser.lexer);
}

pnode_t parser_parse_toplevel() {
    pnode_t node = pnode_listing(pos(), PN_TOPLEVEL);
    while (peek().tt != TT_EOF) {
        pnode_attach(&node, statement());
    }
    if (error_handling.fail == true) {
        pnode_checkfree(&node);
        exit(1);
    }
    return node;
}
