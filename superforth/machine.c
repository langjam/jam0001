#include <stdlib.h>
#include <math.h>
#include "error.h"
#include "machine.h"

static uint64_t longpow(uint64_t base, uint64_t exp) {
	uint64_t result = 1;
	for (;;) {
		if (exp & 1)
			result *= base;
		exp >>= 1;
		if (!exp)
			break;
		base *= base;
	}
	return result;
}

static heap_alloc_t* machine_alloc(machine_t* machine, uint64_t req_size, int trace_children) {
	if (machine->heap_count == machine->heap_alloc_limit)
		PANIC(machine, ERROR_STACK_OVERFLOW);
	heap_alloc_t* heap_alloc = malloc(sizeof(heap_alloc_t));
	PANIC_ON_NULL(heap_alloc, machine, ERROR_OUT_OF_MEMORY);
	PANIC_ON_NULL(heap_alloc->registers = malloc(req_size * sizeof(register_t)), machine, ERROR_OUT_OF_MEMORY);
	PANIC_ON_NULL(heap_alloc->init_stat = calloc(req_size, sizeof(int)), machine, ERROR_OUT_OF_MEMORY);
	heap_alloc->limit = req_size;
	heap_alloc->gc_flag = 0;
	heap_alloc->trace_children = trace_children;
	machine->heap_allocs[machine->heap_count++] = heap_alloc;
	return heap_alloc;
}

static uint64_t machine_heap_trace(machine_t* machine, heap_alloc_t* heap_alloc, heap_alloc_t** reset_stack) {
	uint64_t traced = 0;

	heap_alloc->gc_flag = 1;
	*(reset_stack++) = heap_alloc;

	if (heap_alloc->trace_children)
		for (uint_fast64_t i = 0; i < heap_alloc->limit; i++)
			traced += machine_heap_trace(machine, heap_alloc->registers[i].heap_alloc, reset_stack);
	return traced;
}

static void machine_ins_offset(machine_t* machine, machine_ins_t* instruction) {
	uint64_t offset = machine->global_offset;
	if (instruction->a_flag)
		instruction->a += offset;
	if (instruction->b_flag)
		instruction->b += offset;
	if (instruction->c_flag)
		instruction->c += offset;
}

static const int machine_execute_instruction(machine_t* machine) {
	machine_ins_t ins = *machine->ip;
	machine_ins_offset(machine, &ins);

	switch (ins.op_code)
	{
	case OP_CODE_MOVE:
		machine->stack[ins.a] = machine->stack[ins.b];
		break;
	case OP_CODE_CHECK:
		if (!machine->stack[ins.a].bool_flag)
			machine->ip++;
		break;
	case OP_CODE_JUMP:
		machine->ip = machine->stack[ins.a].ip;
		break;
	case OP_CODE_JUMP_HIST: {
		machine_ins_t* new_loc = machine->stack[ins.a].ip;
		machine->ip = new_loc;
		PANIC_ON_NULL(machine->position_count != machine->frame_limit, machine, ERROR_STACK_OVERFLOW);
		machine->positions[machine->position_count++] = new_loc;
		break;
	}
	case OP_CODE_JUMP_BACK: {
		PANIC_ON_NULL(machine->position_count, machine, ERROR_INSUFFICIENT_POSITIONS);
		machine->ip = machine->positions[--machine->position_count];
		break; 
	}
	case OP_CODE_LOAD_HEAP: {
		register_t array_register = machine->stack[ins.a];
		register_t index_register = machine->stack[ins.b];
		if (index_register.long_int < 0 || index_register.long_int >= array_register.heap_alloc->limit)
			PANIC(machine, ERROR_INDEX_OUT_OF_RANGE);
		if (!array_register.heap_alloc->init_stat[index_register.long_int])
			PANIC(machine, ERROR_READ_UNINIT);
		machine->stack[ins.c] = array_register.heap_alloc->registers[index_register.long_int];
		break;
	}
	case OP_CODE_LOAD_HEAP_I: {
		register_t array_register = machine->stack[ins.a];
		if (!array_register.heap_alloc->init_stat[ins.a])
			PANIC(machine, ERROR_READ_UNINIT);
		machine->stack[ins.c] = array_register.heap_alloc->registers[ins.b];
		break;
	}
	case OP_CODE_STORE_HEAP: {
		register_t array_register = machine->stack[ins.a];
		register_t index_register = machine->stack[ins.b];
		if (index_register.long_int < 0 || index_register.long_int >= array_register.heap_alloc->limit)
			PANIC(machine, ERROR_INDEX_OUT_OF_RANGE);
		array_register.heap_alloc->registers[index_register.long_int] = machine->stack[ins.c];
		array_register.heap_alloc->init_stat[index_register.long_int] = 1;
		break;
	}
	case OP_CODE_STORE_HEAP_I: {
		register_t array_register = machine->stack[ins.a];
		array_register.heap_alloc->registers[ins.b] = machine->stack[ins.c];
		array_register.heap_alloc->init_stat[ins.b] = 1;
		break;
	}
	case OP_CODE_STACK_OFFSET:
		machine->global_offset += ins.a;
		break;
	case OP_CODE_STACK_DEOFFSET:
		machine->global_offset -= ins.a;
		break;
	case OP_CODE_HEAP_ALLOC: {
		ESCAPE_ON_NULL(machine->stack[ins.a].heap_alloc = machine_alloc(machine, ins.b, ins.c));
		break;
	}
	case OP_CODE_HEAP_NEW_FRAME: {
		if (machine->heap_frame == machine->frame_limit)
			PANIC(machine, ERROR_STACK_OVERFLOW);
		machine->heap_frame_bounds[machine->heap_frame++] = machine->heap_count;
		machine->heap_reset_count = 0;
		break;
	}
	case OP_CODE_HEAP_TRACE: {
		machine->heap_reset_count += machine_heap_trace(machine, machine->stack[ins.a].heap_alloc, &machine->heap_reset_stack[machine->heap_reset_count]);
		break;
	}
	case OP_CODE_HEAP_CLEAN: {
		uint64_t kept_allocs = 0;
		for(uint_fast64_t i = machine->heap_frame_bounds[--machine->heap_frame]; i < machine->heap_count; i++)
			if ((*machine->heap_allocs[i]).gc_flag)
				machine->heap_allocs[kept_allocs++] = machine->heap_allocs[i];
			else {
				free(machine->heap_allocs[i]->registers);
				free(machine->heap_allocs[i]->init_stat);
				free(machine->heap_allocs[i]);
			}
		machine->heap_count = machine->heap_frame_bounds[machine->heap_frame] + kept_allocs;
		break;
	}
	case OP_CODE_AND:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].bool_flag && machine->stack[ins.b].bool_flag;
		break;
	case OP_CODE_OR:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].bool_flag || machine->stack[ins.b].bool_flag;
		break;
	case OP_CODE_NOT:
		machine->stack[ins.b].bool_flag = !machine->stack[ins.a].bool_flag;
		break;
	case OP_CODE_BOOL_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].bool_flag == machine->stack[ins.b].bool_flag;
		break;
	case OP_CODE_CHAR_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].char_int == machine->stack[ins.b].char_int;
		break;
	case OP_CODE_LONG_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].long_int == machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_MORE_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].long_int >= machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_LESS_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].long_int <= machine->stack[ins.b].long_int;
		break;
	case OP_CODE_FLOAT_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].float_int == machine->stack[ins.b].float_int;
		break;
	case OP_CODE_FLOAT_MORE_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].float_int >= machine->stack[ins.b].float_int;
		break;
	case OP_CODE_FLOAT_LESS_EQUAL:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].float_int <= machine->stack[ins.b].float_int;
		break;
	case OP_CODE_LONG_MORE:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].long_int > machine->stack[ins.b].long_int;
		break;
	case OP_CODE_FLOAT_MORE:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].float_int > machine->stack[ins.b].float_int;
		break;
	case OP_CODE_LONG_LESS:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].long_int < machine->stack[ins.b].long_int;
		break;
	case OP_CODE_FLOAT_LESS:
		machine->stack[ins.c].bool_flag = machine->stack[ins.a].float_int < machine->stack[ins.b].float_int;
		break;
	case OP_CODE_LONG_ADD:
		machine->stack[ins.c].long_int = machine->stack[ins.a].long_int + machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_SUBRACT:
		machine->stack[ins.c].long_int = machine->stack[ins.a].long_int - machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_MULTIPLY:
		machine->stack[ins.c].long_int = machine->stack[ins.a].long_int * machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_DIVIDE:
		machine->stack[ins.c].long_int = machine->stack[ins.a].long_int / machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_MODULO:
		machine->stack[ins.c].long_int = machine->stack[ins.a].long_int % machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_EXPONENTIATE:
		machine->stack[ins.c].long_int = longpow(machine->stack[ins.a].long_int, machine->stack[ins.b].long_int);
		break;
	case OP_CODE_FLOAT_ADD:
		machine->stack[ins.c].float_int = machine->stack[ins.a].float_int + machine->stack[ins.b].float_int;
		break;
	case OP_CODE_FLOAT_SUBTRACT:
		machine->stack[ins.c].float_int = machine->stack[ins.a].float_int - machine->stack[ins.b].float_int;
		break;
	case OP_CODE_FLOAT_MULTIPLY:
		machine->stack[ins.c].float_int = machine->stack[ins.a].float_int * machine->stack[ins.b].float_int;
		break;
	case OP_CODE_FLOAT_DIVIDE:
		machine->stack[ins.c].float_int = machine->stack[ins.a].float_int / machine->stack[ins.b].float_int;
		break;
	case OP_CODE_FLOAT_MODULO:
		machine->stack[ins.c].float_int = fmod(machine->stack[ins.a].float_int, machine->stack[ins.b].float_int);
		break;
	case OP_CODE_FLOAT_EXPONENTIATE:
		machine->stack[ins.c].float_int = pow(machine->stack[ins.a].float_int, machine->stack[ins.b].float_int);
		break;
	case OP_CODE_LONG_TO_FLOAT:
		machine->stack[ins.a].float_int = machine->stack[ins.b].long_int;
		break;
	case OP_CODE_LONG_NEGATE:
		machine->stack[ins.a].long_int = -machine->stack[ins.b].long_int;
		break;
	case OP_CODE_FLOAT_NEGATE:
		machine->stack[ins.a].float_int = -machine->stack[ins.b].float_int;
		break;
	}
	return 1;
}

const int init_machine(machine_t* machine, uint64_t stack_size, uint64_t heap_alloc_limit, uint64_t frame_limit) {
	machine->heap_alloc_limit = heap_alloc_limit;
	machine->frame_limit = frame_limit;

	machine->global_offset = 0;
	machine->position_count = 0;
	machine->heap_frame = 0;
	machine->heap_count = 0;

	ESCAPE_ON_NULL(machine->stack = malloc(stack_size * sizeof(register_t)));
	ESCAPE_ON_NULL(machine->positions = malloc(machine->frame_limit * sizeof(machine_ins_t*)));
	ESCAPE_ON_NULL(machine->heap_allocs = malloc(machine->heap_alloc_limit * sizeof(heap_alloc_t*)));
	ESCAPE_ON_NULL(machine->heap_frame_bounds = malloc(machine->frame_limit * sizeof(uint64_t)));
	return 1;
}

void free_machine(machine_t* machine) {
	free(machine->stack);
	free(machine->positions);
	free(machine->heap_allocs);
	free(machine->heap_frame_bounds);
}

const int machine_execute(machine_t* machine, machine_ins_t* instructions, uint64_t instruction_count) {
	machine_ins_t* last_ins = &instructions[instruction_count - 1];
	while (machine->ip <= last_ins)
	{
		ESCAPE_ON_NULL(machine_execute_instruction(machine));
		machine->ip++;
	}
	return 1;
}