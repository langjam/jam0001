#pragma once

#ifndef SCANNER_H
#define SCANNER_H

#include <stdint.h>
#include "error.h"
#include "tokens.h"

typedef struct scanner {
	const char* source;
	uint32_t length, position, row, col;

	token_t last_tok;
	char last_char;

	error_t last_err;
} scanner_t;

void init_scanner(scanner_t* scanner, const char* source, const uint32_t length);

const int scanner_read_tok(scanner_t* scanner);

#endif // !SCANNER_H