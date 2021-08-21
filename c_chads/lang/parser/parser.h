#pragma once
#include "../lexer.h"
#include "../../aid/vec/vec.h"

struct Parser_State {
    struct Lexer_State lexer;
    struct Token current_token;
};

enum Parser_Node_Kind {
    PN_DECL,
    PN_STRING,
    PN_IDENT,
    PN_NUMBER,
    PN_PROC,
    PN_CALL,
};

struct Parser_Node {
    enum Parser_Node_Kind kind;
    struct Vec OF(struct Parser_Node) children;
    union {
        struct {
            struct Span name; 
        } decl;
        struct {
            struct Span name; 
        } call;
        struct {
            struct Span val; 
        } string;
        struct {
            struct Span val; 
        } number;
        struct {
            struct Span val; 
        } ident;
    } data;
};

void parser_init(const string src);
struct Parser_Node parser_parse_toplevel();
struct Parser_State *parser_get_state();
void parser_deinit();
