#pragma once

#ifndef ERROR_H

#include <stdint.h>

typedef enum error {
	ERROR_NONE,
	ERROR_OUT_OF_MEMORY,

	//compiler errors
	ERROR_UNEXPECTED_TOK,

	ERROR_NO_AUTO_TYPING,
	ERROR_EXPECTED_SUB_TYPES,
	ERROR_TO_MANY_SUB_TYPES,

	ERROR_UNDECLARED_VAR,
	ERROR_UNEXPECTED_TYPE,
	ERROR_UNEXPECTED_ARGUMENT_LENGTH,

	//virtual-machine errors
	ERROR_INSUFFICIENT_POSITIONS,
	ERROR_INDEX_OUT_OF_RANGE,
	ERROR_STACK_OVERFLOW,
	ERROR_READ_UNINIT,
} error_t;

#define PANIC(OBJ, ERROR){ OBJ->last_err = ERROR; return 0; }
#define ESCAPE_ON_NULL(PTR) {if(!(PTR)) { return 0; }}
#define PANIC_ON_NULL(PTR, OBJ, ERROR) {if(!(PTR)) PANIC(OBJ, ERROR)}

#endif // !ERROR_H
