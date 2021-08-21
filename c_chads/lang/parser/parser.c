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
} error_handling = {
    .expected = NULL,
    .unmatched = 0
};

static rune begindelim(rune delim) {
    rune prev = error_handling.unmatched;
    error_handling.unmatched = delim;
    return prev;
}

static void setexpect(string expect) {
    error_handling.expected = expect;
}

static void enddelim(rune prev) {
    error_handling.unmatched = prev;
}


static void lineinfo(tok_t *token) {
    eh_error(token->line, token->col, parser.lexer.src);
}

static tok_t pull() {
    tok_t tok = parser.current_token;
    parser.current_token = lex_determine(&parser.lexer);
    if (tok.tt == TT_INVALID) {
        EH_MESSAGE("Invalid character: `%.*s`\n", (int) parser.current_token.span.size, parser.lexer.src+parser.current_token.span.from);
        lineinfo(&tok);
        exit(1);
    }
    if (tok.tt == TT_EOF) {
        EH_MESSAGE("Unexpected end of file");
        if (error_handling.expected != NULL) {
            EH_MESSAGE(" (Expected %s)", error_handling.expected);
        }
        if (error_handling.unmatched != 0) {
            EH_MESSAGE(", unclosed `%c`", error_handling.unmatched);
        }
        lineinfo(&parser.current_token);
        if (parser.lexer.src[tok.span.from] == '\"') {
            fprintf(stderr, "(Probably due to unclosed \")\n");
            eh_at_line(tok.line, parser.lexer.src);
            eh_point(tok.col);
        }
        exit(1);
    }
    return tok;
}

tok_t peek() {
    return parser.current_token;
}

bool check(const string against) {
    return spanstreqstr(parser.current_token.span, parser.lexer.src, against);
}

static void stray(tok_t *tok) {
    EH_MESSAGE("Stray `%.*s`", SPAN_PF(tok->span, parser.lexer.src));
    if (error_handling.expected != NULL) {
        EH_MESSAGE(" (Expected %s)", error_handling.expected);
    }
    if (error_handling.unmatched != 0) {
        EH_MESSAGE(", unclosed `%c`", error_handling.unmatched);
    }
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

__attribute__((unused))
static void skip_v(const string v) {
    tok_t tok = pull();
    if (!spanstreqstr(tok.span, parser.lexer.src, v)) {
        stray(&tok);
    }
}
/*
static pnode_t pnode_new(pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .children = vec_new(sizeof(pnode_t))
    };
}*/

static void pnode_attach(pnode_t *left, pnode_t right) {
    vec_push(&left->children, &right);
}

static pnode_t pnode_listing(pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .addressing = PA_LISTING,
        .children = vec_new(sizeof(pnode_t))
    };
}

static pnode_t pnode_binary(pnode_kind_t kind, pnode_t left, pnode_t right) {
    pnode_t node = {
        .kind = kind,
        .addressing = PA_BINARY,
        .children = vec_new(sizeof(pnode_t))
    };
    pnode_attach(&node, left);
    pnode_attach(&node, right);
    return node;
}

pnode_t *pnode_left(pnode_t *of) {
    if (!(of->addressing == PA_BINARY)) {
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

static pnode_t pnode_endpoint(pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .addressing = PA_ENDPOINT,
        .children = vec_new(sizeof(pnode_t))
    };
}

static pnode_t delimited(pnode_kind_t kind, const string open, const string shut, bool mustclose, pnode_t callback()) {
    pnode_t node = pnode_listing(kind);
    rune delim = begindelim(*open);
    skip_v(open);
    while (!check(shut)) {
        pnode_attach(&node, callback());
        if (!check(shut) || mustclose)
            skip_tt(TT_SEMI);
    }
    enddelim(delim);
    skip_v(shut);
    return node;
}


static pnode_t assignment();
static pnode_t value();
static pnode_t maybe_call() {
    pnode_t left = value();
 //   fprintf(stderr, "Token: %.*s\n", (int)parser.current_token.span.size, parser.current_token.span.from+parser.lexer.src);
    if (!check("("))
        return left;
    return pnode_binary(PN_CALL, left, delimited(PN_PARAMS, "(", ")", false, assignment));
}

static pnode_t declaration(tok_t on);

static strview_t typedecl() {
    setexpect("type name");
    tok_t token = pull();
    assert_tt(&token, TT_IDENT);
    setexpect(NULL);
    return strview_span(token.span, parser.lexer.src);
}

static pnode_t assignment() {
    pnode_t left = maybe_call();
    if (peek().tt != TT_ASSIGN)
        return left;
    skip_tt(TT_ASSIGN);
    pnode_t right = maybe_call();
    return pnode_binary(PN_ASSIGN, left, right);
}

static pnode_t pulldeclaration() {
    return declaration(pull());
}

static pnode_t value() {
    tok_t token = pull();
    pnode_t value_node;
    switch (token.tt) {
        case TT_STRING:
            value_node = pnode_endpoint(PN_STRING);
            value_node.data.string.val = strview_span(token.span, parser.lexer.src);
            return value_node;
        case TT_NUMBER:
            value_node = pnode_endpoint(PN_NUMBER);
            value_node.data.number.val = strview_span(token.span, parser.lexer.src);
            return value_node;
        case TT_IDENT:
            if (peek().tt == TT_DEF)
                return declaration(token);
            else {
                value_node = pnode_endpoint(PN_IDENT);
                value_node.data.ident.val = strview_span(token.span, parser.lexer.src);
                return value_node;
            }
        case TT_PROC:
            return pnode_binary(
                PN_PROC, 
                delimited(PN_TYPELIST, "(", ")", false, pulldeclaration),
                delimited(PN_BODY, "{", "}", true, assignment)
            );
        default:
            stray(&token);
            exit(1);
    }
}

static pnode_t declaration(tok_t on) {
    assert_tt(&on, TT_IDENT);
    setexpect("`:` followed by type");
    skip_tt(TT_DEF);
    setexpect(NULL);
    pnode_t decl_node = pnode_endpoint(PN_DECL);
    decl_node.data.decl.name = strview_span(on.span, parser.lexer.src);
    decl_node.data.decl.type = strview_from("_");
    if (peek().tt != TT_ASSIGN) {
        decl_node.data.decl.type = typedecl();
    }
    return decl_node;
}


void parser_init(const string src) {
    parser = (struct Parser_State) {
        .lexer = lex_new(src)
    };
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
        pnode_attach(&node, assignment());
        setexpect("semicolon");
        skip_tt(TT_SEMI);
        setexpect(NULL);
    }
    return node;
}
