#pragma once
#include "../lexer.h"
#include "../eh.h"
#include "../../aid/vec/vec.h"
#include "../../aid/strview/strview.h"

struct Parser_State {
    struct Lexer_State lexer;
    struct Token current_token;
};

enum Parser_Node_Addressing {
    PA_BINARY,
    PA_UNARY,
    PA_LISTING,
    PA_ENDPOINT
};

enum Parser_Node_Kind {
    PN_INVAL,    // ENDPOINT, USED INTERNALLY BY PARSER DONT TOUCH
    PN_RETURN,   // UNARY
    PN_TYPELIST, // LISTING
    PN_UNARY,    // UNARY
    PN_OPERATOR, // BINARY
    PN_IF,       // BINARY
    PN_WHILE,    // BINARY
    PN_PARAMS,   // LISTING
    PN_DECL,     // ENDPOINT 
    PN_BODY,     // LISTING
    PN_TOPLEVEL, // LISTING
    PN_ASSIGN,   // BINARY
    PN_STRING,   // ENDPOINT
    PN_IDENT,    // ENDPOINT
    PN_NUMBER,   // ENDPOINT
    PN_PROC,     // BINARY
    PN_CALL,     // BINARY
};

struct Parser_Node {
    enum Parser_Node_Addressing addressing;
    enum Parser_Node_Kind kind;
    // e.g. comments
    struct Vec OF(struct Parser_Node) children;
    union {
        struct {
            struct Vec OF(strview_t) annotations;
            strview_t type;
            strview_t name;
        } decl;
        struct {
            strview_t return_type;
        } proc;
        struct {
            strview_t val; 
        } string;
        struct {
            strview_t val; 
        } number;
        struct {
            strview_t val; 
        } ident;
        struct {
            strview_t op; 
        } op;
        struct {
            strview_t op; 
        } unary;
    } data;
};

void parser_init(const string src);
struct Parser_Node parser_parse_toplevel();
struct Parser_State *parser_get_state();
struct Parser_Node *pnode_uvalue(struct Parser_Node *of);
struct Parser_Node *pnode_right(struct Parser_Node *of);
struct Parser_Node *pnode_left(struct Parser_Node *of);
void parser_deinit();
