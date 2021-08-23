#pragma once

#ifndef OPCODE_H
#define OPCODE_H

#include <stdint.h>

typedef union machine_register register_t;

typedef enum machine_op_code {
	OP_CODE_MOVE,
	OP_CODE_CHECK,

	OP_CODE_JUMP,
	OP_CODE_JUMP_HIST,
	OP_CODE_JUMP_BACK,

	OP_CODE_LOAD_HEAP,
	OP_CODE_LOAD_HEAP_I,
	OP_CODE_STORE_HEAP,
	OP_CODE_STORE_HEAP_I,

	OP_CODE_STACK_OFFSET,
	OP_CODE_STACK_DEOFFSET,

	OP_CODE_HEAP_ALLOC,
	OP_CODE_HEAP_NEW_FRAME,
	OP_CODE_HEAP_TRACE,
	OP_CODE_HEAP_CLEAN,

	OP_CODE_AND,
	OP_CODE_OR,
	OP_CODE_NOT,
	
	OP_CODE_BOOL_EQUAL,
	OP_CODE_CHAR_EQUAL,
	OP_CODE_LONG_EQUAL,
	OP_CODE_FLOAT_EQUAL,

	OP_CODE_LONG_MORE,
	OP_CODE_LONG_LESS,
	OP_CODE_LONG_MORE_EQUAL,
	OP_CODE_LONG_LESS_EQUAL,

	OP_CODE_LONG_ADD,
	OP_CODE_LONG_SUBRACT,
	OP_CODE_LONG_MULTIPLY,
	OP_CODE_LONG_DIVIDE,
	OP_CODE_LONG_MODULO,
	OP_CODE_LONG_EXPONENTIATE,

	OP_CODE_FLOAT_MORE,
	OP_CODE_FLOAT_LESS,
	OP_CODE_FLOAT_MORE_EQUAL,
	OP_CODE_FLOAT_LESS_EQUAL,

	OP_CODE_FLOAT_ADD,
	OP_CODE_FLOAT_SUBTRACT,
	OP_CODE_FLOAT_MULTIPLY,
	OP_CODE_FLOAT_DIVIDE,
	OP_CODE_FLOAT_MODULO,
	OP_CODE_FLOAT_EXPONENTIATE,

	OP_CODE_LONG_NEGATE,
	OP_CODE_FLOAT_NEGATE,
	OP_CODE_LONG_TO_FLOAT,
} op_code_t;

typedef struct machine_instruction {
	op_code_t op_code;
	uint64_t a, b, c;
	int a_flag, b_flag, c_flag;
} machine_ins_t;

typedef struct machine_heap_alloc {
	register_t* registers;
	int* init_stat;
	uint32_t limit;

	int gc_flag, trace_children;
} heap_alloc_t;

typedef union machine_register {
	heap_alloc_t* heap_alloc;
	int64_t long_int;
	double float_int;
	char char_int;
	int bool_flag;
	machine_ins_t* ip;
} register_t;

typedef struct machine {
	register_t* stack;

	machine_ins_t* ip;
	machine_ins_t** positions;

	heap_alloc_t* heap_reset_stack[64];
	heap_alloc_t** heap_allocs;
	uint64_t* heap_frame_bounds;

	error_t last_err;
	
	uint64_t global_offset, position_count, heap_frame, heap_count, heap_reset_count, heap_alloc_limit, frame_limit;
} machine_t;

const int init_machine(machine_t* machine, uint64_t stack_size, uint64_t heap_alloc_limit, uint64_t frame_limit);
void free_machine(machine_t* machine);

const int machine_execute(machine_t* machine, machine_ins_t* instructions, uint64_t instruction_count);

#endif // !OPCODE_H