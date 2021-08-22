#pragma once

#ifndef COMPILER_H
#define COMPILER_H

#include "error.h"
#include "ast.h"
#include "machine.h"

typedef struct compiler {
	ast_t ast;
	error_t last_err;
} compiler_t;

const int init_compiler(compiler_t* compiler, const char* source);
void free_compiler(compiler_t* compiler);

const int compile(compiler_t* compiler, machine_t* machine);

#endif // !COMPILER_H
