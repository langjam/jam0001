#include "parser.h"
#include <stdio.h>
typedef struct Parser_Node pnode_t;
typedef struct Parser_State pstate_t;
typedef struct Token tok_t;
typedef enum Parser_Node_Kind pnode_kind_t;

static struct Parser_State parser;
struct {
    rune unmatched;
    string expected;
    bool fail;
} error_handling = {
    .expected = NULL,
    .unmatched = 0,
    .fail = false,
};

struct Vec OF(strview_t) preceding_comments;

static void add_comments() {
    // Nocom
    struct Span spn = lex_get_comment(&parser.lexer);
    while (!(spn.from == 0 && spn.size == 0)) {
        strview_t view = strview_span(spn, parser.lexer.src);
        printf("Adding comment %.*s\n", (int)view.size, view.view);
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

static pnode_t inval() {
    EH_MESSAGE("Invalid expression");
    lineinfo(&parser.current_token);
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

tok_t peek() {
    return parser.current_token;
}

bool check(const string against) {
    return spanstreqstr(parser.current_token.span, parser.lexer.src, against);
}

static void stray_panic(tok_t *tok) {
    if (tok->tt == TT_EOF)
        eof(*tok);
    EH_MESSAGE("Stray `%.*s`", SPAN_PF(tok->span, parser.lexer.src));
    lineinfo(tok);
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

static void skip_tt(enum Token_Type tt) {
    tok_t tok = pull();
    if (tok.tt != tt) {
        stray(&tok);
    }
}

static void skip_tt_panic(const string shut) {
    tok_t tok = peek();
    if (tok.tt != TT_SEMI) {
        error_handling.fail = true;
        if (tok.tt != TT_INVALID)
            stray_panic(&tok);
        while (!(peek().tt == TT_SEMI || check(shut))) {
            pull();
        };
    }
    else 
        pull();
}

__attribute__((unused))
static void skip_v(const string v) {
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

static void pnode_attach(pnode_t *left, pnode_t right) {
    if (isinval(right)) pnode_checkfree(left);
    if (isinval(*left)) return;
    vec_push(&left->children, &right);
}

static pnode_t pnode_listing(pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .addressing = PA_LISTING,
        .children = vec_new(sizeof(pnode_t))
    };
}

static pnode_t pnode_unary(pnode_kind_t kind, pnode_t left) {
    if (isinval(left)) {
        return mkinval();
    }
    pnode_t node = {
        .kind = kind,
        .addressing = PA_UNARY,
        .children = vec_new(sizeof(pnode_t))
    };
    pnode_attach(&node, left);
    return node;
}

static pnode_t pnode_binary(pnode_kind_t kind, pnode_t left, pnode_t right) {
    if (isinval(left) || isinval(right)) {
        pnode_checkfree(&left);
        pnode_checkfree(&right);
        return mkinval();
    }
    pnode_t node = {
        .kind = kind,
        .addressing = PA_BINARY,
        .children = vec_new(sizeof(pnode_t))
    };
    pnode_attach(&node, left);
    pnode_attach(&node, right);
    return node;
}

static pnode_t pnode_ternary(pnode_kind_t kind, pnode_t cond, pnode_t left, pnode_t right) {
    if (isinval(cond) || isinval(left) || isinval(right)) {
        pnode_checkfree(&cond);
        pnode_checkfree(&left);
        pnode_checkfree(&right);
        return mkinval();
    }
    pnode_t node = {
        .kind = kind,
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
    return vec_get(&of->children, 1);
}

pnode_t *pnode_left(pnode_t *of) {
    if (of->addressing != PA_BINARY) {
        fprintf(stderr, "Internal error: Invalid addressing mode for binary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 2);
}

pnode_t *pnode_right(pnode_t *of) {
    if (!(of->addressing == PA_BINARY)) {
        fprintf(stderr, "Internal error: Invalid addressing mode for binary expression, %d", of->addressing);
        exit(1);
    }
    return vec_get(&of->children, 1);
}

static pnode_t pnode_endpoint(pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .addressing = PA_ENDPOINT,
    };
}

static pnode_t delimited(pnode_kind_t kind, const string open, const string shut, bool mustclose, bool autohandle, pnode_t callback()) {
    if (kind == PN_TYPELIST) 
        setexpect("Did you mean to create a parameter list?");
    pnode_t node = pnode_listing(kind);
    skip_v(open);
    setexpect(NULL);
    rune delim = begindelim(*open);
    while (!check(shut)) {
        pnode_t v = callback();
        add_comments();
        pnode_attach(&node, v);
        if (check(shut) && (!mustclose)) break;
        if (isinval(v)) {
            setexpect("Invalid expression");
            skip_tt_panic(shut);
        }
        if (!autohandle)
            if (!check(shut) || mustclose) { 
                setexpect("Expected semicolon befor it");
                skip_tt_panic(shut);
            }
    }
    setexpect(NULL);
    enddelim(delim);
    skip_v(shut);
    return node;
}


static pnode_t most_important_expression();
static pnode_t value();
static pnode_t maybe_call() {
    pnode_t left = value();
    if (!check("("))
        return left;
    return pnode_binary(PN_CALL, left, delimited(PN_PARAMS, "(", ")", false, false, most_important_expression));
}

static pnode_t declaration(tok_t on);

static strview_t typedecl() {
    setexpect("Expected type name");
    tok_t token = pull();
    assert_tt(&token, TT_IDENT);
    setexpect(NULL);
    return strview_span(token.span, parser.lexer.src);
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
    while (peek().tt == TT_OPERATOR && getprec(peek().span) == prec) {
        pnode_kind_t kind = PN_OPERATOR;
        struct Span op = peek().span;
        skip_tt(TT_OPERATOR);
        if (spanstreqstr(op, parser.lexer.src, "="))
            kind = PN_ASSIGN;
        pnode_t right = parse_expr(prec-1);
        left = pnode_binary(kind, left, right);
        left.data.op.op = strview_span(op, parser.lexer.src); 
    }
    return left;
}

static pnode_t most_important_expression() {
    return parse_expr(OPS_H);
}

/*
static pnode_t maybe_assignment() {
    pnode_t left = maybe_call();
    if (peek().tt != TT_ASSIGN)
        return left;
    skip_tt(TT_ASSIGN);
    pnode_t right = maybe_call();
    return pnode_binary(PN_ASSIGN, left, right);
}*/

static pnode_t pulldeclaration() {
    return declaration(pull());
}

static pnode_t statement();

static pnode_t body() {
    return delimited(PN_BODY, "{", "}", true, true, statement);
}

static pnode_t statement() {
    if (check("{")) {
        return body();
    }
    else if (check("while")) {
        skip_v("while");
        rune delim = begindelim('(');
        skip_v("(");
        pnode_t left = most_important_expression();
        skip_v(")");
        enddelim(delim);
        return pnode_binary(PN_WHILE, left, statement());
    }
    else if (check("return")) {
        skip_v("return");
        pnode_t node = pnode_unary(PN_RETURN, most_important_expression());
        skip_v(";");
        return node;
    }
    else if (check("if")) {
        skip_v("if");
        rune delim = begindelim('(');
        skip_v("(");
        pnode_t cond = most_important_expression();
        skip_v(")");
        enddelim(delim);
        pnode_t body = statement();
        if (check("else")) {
            skip_v("else");
            return pnode_ternary(PN_IF, cond, body, statement());
        }
        else {
            return pnode_binary(PN_IF, cond, body);
        }
    }
    pnode_t node = most_important_expression();
    setexpect("Expected semicolon before it");
    skip_tt(TT_SEMI);
    return node;
}

static pnode_t value() {
    tok_t token = peek();
    pnode_t value_node;
    enum Parser_Number_Kind kind;
    switch (token.tt) {
        case TT_STRING:
            pull();
            value_node = pnode_endpoint(PN_STRING);
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
            value_node = pnode_endpoint(PN_NUMBER);
            value_node.data.number.val = strview_span(token.span, parser.lexer.src);
            value_node.data.number.kind = kind;
            return value_node;
        case TT_IDENT: {
            pull();
            if (peek().tt == TT_DEF) {
                return declaration(token);
            }
            else {
                value_node = pnode_endpoint(PN_IDENT);
                value_node.data.ident.val = strview_span(token.span, parser.lexer.src);
                return value_node;
            }
        }
        case TT_PROC:
            pull();
            pnode_t left = delimited(PN_TYPELIST, "(", ")", false, false, pulldeclaration);
            strview_t returntype = typedecl();
            value_node = pnode_binary(
                PN_PROC, 
                left,
                body()
            );
            value_node.data.proc.return_type = returntype;
            return value_node;
        case TT_LPAREN:
            skip_tt(TT_LPAREN);
            value_node = most_important_expression();
            skip_tt(TT_RPAREN);
            return value_node;
        case TT_LBRACE:
            value_node = body();
            return value_node;
        case TT_OPERATOR:
            if (
                spanstreqstr(token.span, parser.lexer.src, "!") ||
                spanstreqstr(token.span, parser.lexer.src, "-")
            ) {
                pull();
                value_node = pnode_unary(PN_UNARY, value());
                value_node.data.unary.op = strview_span(token.span, parser.lexer.src);
                return value_node;
            }
        default: {}
    }
    return inval();
}

static pnode_t declaration(tok_t on) {
    assert_tt(&on, TT_IDENT);
    setexpect("Expected `:` followed by type");
    skip_tt(TT_DEF);
    setexpect(NULL);
    pnode_t decl_node = pnode_endpoint(PN_DECL);
    decl_node.data.decl.name = strview_span(on.span, parser.lexer.src);
    decl_node.data.decl.type = strview_from("_");
    if (!check("=")) {
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
    pnode_t node = pnode_listing(PN_TOPLEVEL);
    while (peek().tt != TT_EOF) {
        pnode_attach(&node, statement());
    }
    if (error_handling.fail == true) {
        pnode_checkfree(&node);
        exit(1);
    }
    return node;
}
