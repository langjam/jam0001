#pragma once

#ifndef TOKEN_H
#define TOKEN_H

#include <stdint.h>

typedef enum token_type {
	TOK_EOF,

	TOK_TYPECHECK_BOOL,
	TOK_TYPECHECK_CHAR,
	TOK_TYPECHECK_LONG,
	TOK_TYPECHECK_FLOAT,
	TOK_TYPECHECK_ARRAY,
	TOK_TYPECHECK_PROC,
	TOK_AUTO,
	TOK_GLOBAL,

	TOK_IF,
	TOK_ELIF,
	TOK_ELSE,
	TOK_WHILE,

	TOK_NEW,
	TOK_RETURN,
	TOK_SET,

	TOK_TRUE,
	TOK_FALSE,

	TOK_IDENTIFIER,

	TOK_NUMERICAL,
	TOK_STRING,
	TOK_CHAR,

	TOK_EQUALS,
	TOK_NOT_EQUAL,

	TOK_MORE,
	TOK_LESS,
	TOK_MORE_EQUAL,
	TOK_LESS_EQUAL,

	TOK_ADD,
	TOK_SUBTRACT,
	TOK_MULTIPLY,
	TOK_DIVIDE,
	TOK_MODULO,
	TOK_POWER,

	TOK_AND,
	TOK_OR,
	TOK_NOT,

	TOK_OPEN_BRACE,
	TOK_CLOSE_BRACE,
	TOK_OPEN_PAREN,
	TOK_CLOSE_PAREN,
	TOK_OPEN_BRACKET,
	TOK_CLOSE_BRACKET,

	TOK_COMMA
} token_type_t;

typedef struct token {
	token_type_t type;

	const char* str;
	uint32_t length;
} token_t;

#endif // !TOKEN_H