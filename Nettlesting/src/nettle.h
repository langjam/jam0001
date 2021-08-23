#ifndef _NETTLE_H_
#define _NETTLE_H_
#include <stdbool.h>
#include "named_enum.h"
#include "ext/sheredom/hashmap.h"
#include "ext/rxi/vec.h"
typedef vec_t(struct nettle_token) vec_nettle_token_t;

struct nettle_token {
    int type;
    union {
        struct { long start; long end; } multi;
        struct { long position; char c; } single;
        struct { long value; } lit_number;
    };
};

struct nettle_file {
    long index;
    long length;
    char *source;
    
    long lexer_comment_depth;
    
    int lexer_number_base;
    long line_x;
    long line_y;
};

enum {
    CMFLG_EMPTY,
    CMFLG_UNBOUND,
    CMFLG_BOUND,
    CMFLG_INVALID
};

enum {
    CMBND_EMPTY,
    CMBND_STRING,
    CMBND_NUMBER,
    CMBND_STRING_ARRAY,
    CMBND_NUMBER_ARRAY
};
enum {
    CMVIS_GLOBAL,
    CMVIS_LOCAL
};

enum {
    PT_EMPTY,
    PT_NUMBER,
    PT_STRING,
    PT_IDENTIFIER,
    PT_NUMBERS,
    PT_STRINGS,

    PT_OPERATOR
};
struct nettle_parser_token;
//typedef vec_t(int) vec_nettle_operator_t;
//typedef vec_t(struct nettle_parser_token*) vec_nettle_parser_token_t;
struct nettle_parser_expression {
    struct nettle_parser_token *token;
    struct nettle_parser_expression *children[2];
};
struct nettle_parser_token {
    int type;
    union {
        struct { long value; } number;
        struct { char *value; } string;
        struct { char *value; } identifier;
        struct { long *values; int count; int size; } numbers;
        struct { char *values; int count; int size; } strings;
        struct { int type; } operator;
    };
};

struct nettle_parser_code_block; 

/* comment block */
struct nettle_parser_comment_block {
    struct hashmap_s *map;
    int flags;
    int visibility;
    int bind_type;
    union {
        struct nettle_parser_code_block *bound_block;
        struct nettle_parser_token      *bound_token;
    };
};
struct nettle_parser_comment_block_entry {
    char *name;
    struct nettle_parser_token token;
};
struct nettle_parser_comment_block_definition {
    char *name;
    struct nettle_parser_expression *expression_tree;
};

typedef vec_t(struct nettle_parser_comment_block*) vec_nettle_parser_comment_block_t;
typedef vec_t(struct nettle_parser_comment_block_definition*) vec_nettle_parser_comment_block_definition_t;
/* code block */
typedef vec_t(struct nettle_parser_statement*) vec_nettle_parser_statement_t;
struct nettle_parser_code_block {
    vec_nettle_parser_comment_block_t local_comment_blocks;
    vec_nettle_parser_statement_t statements;    
};
enum {
    STMT_LET,
    STMT_IF,
    STMT_ELSEIF,
    STMT_WHILE,
    STMT_COMMENT,
    STMT_CALL
};
struct nettle_parser_statement;


struct nettle_while_statement {
    struct nettle_parser_expression *expression;
    struct nettle_parser_code_block *statements;
};
struct nettle_if_statement {
    struct nettle_parser_expression *expression;
    struct nettle_parser_code_block *statements;

    // chain of if/elseifs
    struct nettle_if_statement *next;
};

struct nettle_parser_statement {
    int type;
    union {
        struct nettle_if_statement *data_if;
        struct nettle_while_statement *data_while;
        struct { struct nettle_parser_token *token; struct nettle_parser_expression *expression; } data_let;
    };

    bool is_bound;
    struct nettle_parser_comment_block_t *bound_block;
};

/* main state */ 
struct nettle_parser_state {
    vec_nettle_parser_comment_block_t global_comment_blocks;
    struct nettle_parser_expression *current_expression;
};
#define DEF_TOKENS(o) \
    o(T_EMPTY, 0) \
    o(T_NPARENL, YT_NPARENL) \
    o(T_NPARENR, YT_NPARENR) \
    o(T_CPARENL, YT_CPARENL) \
    o(T_CPARENR, YT_CPARENR) \
    o(T_SPARENL, YT_SPARENL) \
    o(T_SPARENR, YT_SPARENR) \
    o(T_SEMICOLON, YT_SEMICOLON) \
    o(T_HASH, YT_HASH) \
    o(T_PLUS, YT_PLUS) \
    o(T_MINUS, YT_MINUS) \
    o(T_MULTIPLY, YT_MULTIPLY) \
    o(T_DIVIDE, YT_DIVIDE) \
    o(T_MODULO, YT_MODULO) \
    o(T_XOR, YT_XOR) \
    o(T_COLON, YT_COLON) \
    o(T_EQUAL, YT_EQUAL) \
    o(T_INEQUAL, YT_INEQUAL) \
    o(T_NOT, YT_NOT) \
    o(T_GTHAN, YT_GTHAN) \
    o(T_LTHAN, YT_LTHAN) \
    o(T_GETHAN, YT_GETHAN) \
    o(T_LETHAN, YT_LETHAN) \
    o(T_LSHIFT, YT_LSHIFT) \
    o(T_RSHIFT, YT_RSHIFT) \
    o(T_LAND, YT_LAND) \
    o(T_LOR, YT_LOR) \
    o(T_BAND, YT_BAND) \
    o(T_BOR, YT_BOR) \
    o(T_ASSIGN, YT_ASSIGN) \
    o(T_STRING, YT_STRING) \
    o(T_INTEGER, YT_INTEGER) \
    o(T_IDENTIFIER, YT_IDENTIFIER) \
    o(T_COMMENT, YT_COMMENT) \
    o(T_WHILE, YT_WHILE) \
    o(T_IF, YT_IF) \
    o(T_LET, YT_LET) \
    o(T_RETURN, YT_RETURN) \
    o(T_CONTINUE, YT_CONTINUE) \
    o(T_ELSEIF, YT_ELSEIF) \
    o(T_BREAK, YT_BREAK)


GENERATE_NAMED_ENUM_H(tokens, DEF_TOKENS);
GENERATE_LUT_TABLE_H(tokens, DEF_TOKENS);

extern void nettle(char *src, long src_length);
#endif