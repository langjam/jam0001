#include "parser.h"
#include <stdio.h>
typedef struct Parser_Node pnode_t;
typedef struct Parser_State pstate_t;
typedef struct Token tok_t;
typedef enum Parser_Node_Kind pnode_kind_t;

static struct Parser_State parser;



static tok_t pull() {
    tok_t tok = parser.current_token;
    parser.current_token = lex_determine(&parser.lexer);
    if (tok.tt == TT_INVALID) {
        fprintf(stderr, "Invalid token: `%.*s`\n", (int) parser.current_token.span.size, parser.lexer.src+parser.current_token.span.from);
        exit(1);
    }
    if (tok.tt == TT_EOF) {
        fprintf(stderr, "Unexpected end of file %zu:%zu\n", parser.lexer.line+1, parser.lexer.col+1);
        exit(1);
    }
    return tok;
}

tok_t peek() {
    return parser.current_token;
}

static void assert_tt(tok_t *tok, enum Token_Type tt) {
    if (tok->tt != tt) {
        fprintf(stderr, "Expected %s got %s", TT_NAMES[tt], TT_NAMES[tok->tt]);
        exit(1);
    }
}

static void skip_tt(enum Token_Type tt) {
    tok_t tok = pull();
    if (tok.tt != tt) {
        fprintf(stderr, "Expected %s got %s", TT_NAMES[tt], TT_NAMES[tok.tt]);
        exit(1);
    }
}

static void skip_v(const string v) {
    tok_t tok = pull();
    if (!spanstreqstr(tok.span, parser.lexer.src, v)) {
        fprintf(stderr, "Expected `%s` got `%.*s`", v, (int)tok.span.size, parser.lexer.src+tok.span.from);
        exit(1);
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



static pnode_t value() {
    // We only parse procedures for now
    skip_v("proc");
    skip_tt(TT_LBRACE);
    skip_tt(TT_RBRACE);
    return pnode_new(PN_PROC);
}

static pnode_t declaration() {
    tok_t name = pull();
    assert_tt(&name, TT_IDENT);
    skip_tt(TT_ASSIGN);
    pnode_t right = value();
    pnode_t decl_node = pnode_new(PN_DECL);
    // Attach value
    pnode_attach(&decl_node, right);
    decl_node.data.decl.name = name.span;
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
    return declaration();      
}
