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

static void stray(tok_t *tok) {
    EH_MESSAGE("Stray `%.*s`", SPAN_PF(tok->span, parser.lexer.src));
    if (error_handling.expected != NULL) {
        EH_MESSAGE(" (Expected %s)", error_handling.expected);
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

static pnode_t pnode_new(pnode_kind_t kind) {
    return (pnode_t) {
        .kind = kind,
        .children = vec_new(sizeof(pnode_t))
    };
}

static void pnode_attach(pnode_t *left, pnode_t right) {
    vec_push(&left->children, &right);
}

static pnode_t value();
static pnode_t call(tok_t on) {
    assert_tt(&on, TT_IDENT);
    skip_tt(TT_LPAREN);
    rune delim = begindelim('(');
    pnode_t call_node = pnode_new(PN_CALL);
    call_node.data.call.name = strview_span(on.span, parser.lexer.src);
    while (peek().tt != TT_RPAREN) {
        pnode_attach(&call_node, value());  
        if (peek().tt != TT_RPAREN)
            skip_tt(TT_SEMI);
    }
    skip_tt(TT_RPAREN);
    enddelim(delim);
    return call_node;
}

static pnode_t declaration(tok_t on);

static strview_t typedecl() {
    setexpect("type name");
    tok_t token = pull();
    assert_tt(&token, TT_IDENT);
    return strview_span(token.span, parser.lexer.src);
}

static pnode_t value() {
    tok_t token = pull();
    pnode_t value_node;
    rune delim;
    switch (token.tt) {
        case TT_STRING:
            value_node = pnode_new(PN_STRING);
            value_node.data.string.val = strview_span(token.span, parser.lexer.src);
            return value_node;
        case TT_NUMBER:
            value_node = pnode_new(PN_NUMBER);
            value_node.data.number.val = strview_span(token.span, parser.lexer.src);
            return value_node;
        case TT_IDENT:
            if (peek().tt == TT_ASSIGN)
                return declaration(token);
            if (peek().tt == TT_LPAREN)
                return call(token);
            else {
                value_node = pnode_new(PN_IDENT);
                value_node.data.ident.val = strview_span(token.span, parser.lexer.src);
                return value_node;
            }
        case TT_PROC:
            value_node = pnode_new(PN_PROC);
            skip_tt(TT_LBRACE);
            delim = begindelim('{');
            while (peek().tt != TT_RBRACE) {
                // For now we are just gonna parse some calls
                pnode_attach(&value_node, value());
                skip_tt(TT_SEMI);
            }
            skip_tt(TT_RBRACE);
            enddelim(delim);
            return value_node;
        default:
            stray(&token);
            exit(1);
    }
}

static pnode_t declaration(tok_t on) {
    assert_tt(&on, TT_IDENT);
    setexpect("`:` followed by type");
    if (peek().tt == TT_ASSIGN)
        setexpect("`:` followed by type, note that you need to specify type before assigning");
    skip_tt(TT_DEF);
    strview_t type = typedecl();
    skip_tt(TT_ASSIGN);
    pnode_t right = value();
    pnode_t decl_node = pnode_new(PN_DECL);
    // Attach value
    pnode_attach(&decl_node, right);
    decl_node.data.decl.name = strview_span(on.span, parser.lexer.src);
    decl_node.data.decl.type = type;
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
    pnode_t node = pnode_new(PN_TOPLEVEL);
    while (peek().tt != TT_EOF)
        pnode_attach(&node, declaration(pull()));
    return node;
}
