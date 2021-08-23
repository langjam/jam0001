#include <stdlib.h>
#include "compiler.h"

#define TEMPREG(INDEX) (ast_register_t){.index = INDEX, .offset_flag = 1}
#define GLOBREG(INDEX) (ast_register_t){.index = INDEX, .offset_flag = 0}

#define INS1(OP, REGA) (machine_ins_t){.op_code = OP, .a = REGA.index, .a_flag = REGA.offset_flag}
#define INS2(OP, REGA, REGB) (machine_ins_t){.op_code = OP, .a = REGA.index, .a_flag = REGA.offset_flag, .b = REGB.index, .b_flag = REGB.offset_flag}
#define INS3(OP, REGA, REGB, REGC) (machine_ins_t){.op_code = OP, .a = REGA.index, .a_flag = REGA.offset_flag, .b = REGB.index, .b_flag = REGB.offset_flag, .c = REGC.index, .c_flag = REGC.offset_flag}

#define PUSH_INS(INS) PANIC_ON_NULL(ins_builder_append_ins(ins_builder, INS), compiler, ERROR_OUT_OF_MEMORY)

typedef struct ins_builder {
	machine_ins_t* instructions;

	struct ins_builder_ip {
		machine_ins_t** stack_loc;
		uint64_t index;
	}* ips;

	uint64_t instruction_count, ip_count, alloced_ins, alloced_ips;
} ins_builder_t;

static void compile_ast_code_block(compiler_t* compiler, machine_t* machine, ast_code_block_t* code_block, uint64_t* current_prim_reg);

static const int init_ins_builder(ins_builder_t* ins_builder) {
	ESCAPE_ON_NULL(ins_builder->instructions = malloc((ins_builder->alloced_ins = 64) * sizeof(machine_ins_t)));
	ESCAPE_ON_NULL(ins_builder->ips = malloc((ins_builder->alloced_ips = 16) * sizeof(struct ins_builder_ip)));
	ins_builder->ip_count = 0;
	ins_builder->instruction_count = 0;
	return 1;
}

static void finalize_ins_builder(ins_builder_t* ins_builder) {
	for (uint_fast64_t i = 0; i < ins_builder->ip_count; i++)
		*ins_builder->ips[i].stack_loc = &ins_builder->instructions[ins_builder->ips[i].index];
	free(ins_builder->ips);
}

static const int ins_builder_append_ins(ins_builder_t* ins_builder, machine_ins_t ins) {
	if (ins_builder->instruction_count == ins_builder->alloced_ins) {
		machine_ins_t* new_ins = realloc(ins_builder->instructions, (ins_builder->alloced_ins *= 2) * sizeof(machine_ins_t));
		ESCAPE_ON_NULL(new_ins);
		ins_builder->instructions = new_ins;
	}
	ins_builder->instructions[ins_builder->instruction_count++] = ins;
	return 1;
}

static uint64_t* ins_builder_append_label(ins_builder_t* ins_builder, machine_ins_t** stack_loc, uint64_t loc) {
	if (ins_builder->ip_count == ins_builder->alloced_ips) {
		struct ins_builder_ip* new_ips = realloc(ins_builder->ips, (ins_builder->alloced_ips *= 2) * sizeof(struct ins_builder_ip));
		ESCAPE_ON_NULL(new_ips);
		ins_builder->ips = new_ips;
	}
	ins_builder->ips[ins_builder->ip_count] = (struct ins_builder_ip){
		.index = loc,
		.stack_loc = stack_loc
	};
	return &ins_builder->ips[ins_builder->ip_count++].index;
}

static void compile_ast_prim(compiler_t* compiler, machine_t* machine, ast_value_t* ast_value, uint64_t* current_prim_reg) {
	switch (ast_value->value_type)
	{
	case AST_VALUE_BOOL:
		machine->stack[*current_prim_reg].bool_flag = ast_value->data.bool_flag;
		ast_value->alloced_reg.index = (*current_prim_reg)++;
		ast_value->alloced_reg.offset_flag = 0;
		break;
	case AST_VALUE_CHAR:
		machine->stack[*current_prim_reg].char_int = ast_value->data.character;
		ast_value->alloced_reg.index = (*current_prim_reg)++;
		ast_value->alloced_reg.offset_flag = 0;
		break;
	case AST_VALUE_LONG:
		machine->stack[*current_prim_reg].long_int = ast_value->data.long_int;
		ast_value->alloced_reg.index = (*current_prim_reg)++;
		ast_value->alloced_reg.offset_flag = 0;
		break;
	case AST_VALUE_FLOAT:
		machine->stack[*current_prim_reg].float_int = ast_value->data.float_int;
		ast_value->alloced_reg.index = (*current_prim_reg)++;
		ast_value->alloced_reg.offset_flag = 0;
		break;
	case AST_VALUE_PROC:
		compile_ast_code_block(compiler, machine, &ast_value->data.procedure->exec_block, current_prim_reg);
		break;
	case AST_VALUE_BINARY_OP:
		compile_ast_prim(compiler, machine, &ast_value->data.binary_op->lhs, current_prim_reg);
		compile_ast_prim(compiler, machine, &ast_value->data.binary_op->rhs, current_prim_reg);
		break;
	case AST_VALUE_UNARY_OP:
		compile_ast_prim(compiler, machine, &ast_value->data.unary_op->operand, current_prim_reg);
		break;
	case AST_VALUE_ARRAY_LITERAL: {
		for (uint_fast32_t i = 0; i < ast_value->data.array_literal.element_count; i++)
			compile_ast_prim(compiler, machine, &ast_value->data.array_literal.elements[i], current_prim_reg);
		break;
	case AST_VALUE_PROC_CALL:
		for (uint_fast8_t i = 0; i < ast_value->data.proc_call->argument_count; i++)
			compile_ast_prim(compiler, machine, &ast_value->data.proc_call->arguments[i], current_prim_reg);
		break;
	}
	}
}

static void compile_ast_code_block(compiler_t* compiler, machine_t* machine, ast_code_block_t* code_block, uint64_t* current_prim_reg) {
	for (uint_fast32_t i = 0; i < code_block->instruction_count; i++) {
		switch (code_block->instructions[i].type)
		{
		case AST_TOP_LEVEL_DECL_VAR:
			compile_ast_prim(compiler, machine, &code_block->instructions[i].data.var_decl.set_value, current_prim_reg);
			break;
		case AST_TOP_LEVEL_COND: {
			ast_cond_t* current_conditional = code_block->instructions[i].data.conditional;
			while (current_conditional) {
				compile_ast_code_block(compiler, machine, &current_conditional->exec_block, current_prim_reg);
				if (current_conditional->next_if_false)
					current_conditional = current_conditional->next_if_false;
				else if (current_conditional != current_conditional->next_if_true)
					current_conditional = current_conditional->next_if_true;
				else
					break;
			}
			break;
		}
		case AST_TOP_LEVEL_RETURN:
		case AST_TOP_LEVEL_VALUE:
			compile_ast_prim(compiler, machine, &code_block->instructions[i].data.value, current_prim_reg);
			break;
		}
	}
}

const int init_compiler(compiler_t* compiler, const char* source) {
	PANIC_ON_NULL(init_ast(&compiler->ast, source), compiler, compiler->ast.last_err);
	compiler->last_err = ERROR_NONE;
	return 1;
}

void free_compiler(compiler_t* compiler) {
	free_ast(&compiler->ast);
}

const int compile_ast_value(compiler_t* compiler, ins_builder_t* ins_builder, ast_value_t* value, ast_register_t out_reg, uint64_t temp_regs) {
	switch (value->value_type)
	{
	case AST_VALUE_SET_VAR:
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.set_var->set_value, value->alloced_reg, temp_regs));
	case AST_VALUE_BOOL:
	case AST_VALUE_CHAR:
	case AST_VALUE_LONG:
	case AST_VALUE_FLOAT:
	case AST_VALUE_VAR:
		PUSH_INS(INS2(OP_CODE_MOVE, out_reg, value->alloced_reg));
		break;
	case AST_VALUE_ALLOC_ARRAY:
		PUSH_INS(INS3(OP_CODE_HEAP_ALLOC, out_reg, GLOBREG(value->data.array_literal.element_count), GLOBREG(value->data.array_literal.elem_type.sub_types[0].type != TYPE_SUPER_ARRAY)));
		break;
	case AST_VALUE_ARRAY_LITERAL: {
		PUSH_INS(INS3(OP_CODE_HEAP_ALLOC, out_reg, GLOBREG(value->data.array_literal.element_count), GLOBREG(value->data.array_literal.elem_type.sub_types[0].type != TYPE_SUPER_ARRAY)));
		for (uint_fast32_t i = 0; i < value->data.array_literal.element_count; i++) {
			ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.array_literal.elements[i], TEMPREG(temp_regs), temp_regs + 1));
			PUSH_INS(INS3(OP_CODE_STORE_HEAP_I, out_reg, GLOBREG(i), TEMPREG(temp_regs)));
		}
		break;
	}
	case AST_VALUE_GET_INDEX:
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.get_index->array, TEMPREG(temp_regs), temp_regs + 1));
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.get_index->index, TEMPREG(temp_regs + 1), temp_regs + 2));
		PUSH_INS(INS3(OP_CODE_LOAD_HEAP, TEMPREG(temp_regs), TEMPREG(temp_regs + 1), out_reg));
		break;
	case AST_VALUE_SET_INDEX:
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.set_index->array, TEMPREG(temp_regs), temp_regs + 1));
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.set_index->index, TEMPREG(temp_regs + 1), temp_regs + 2));
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.set_index->value, TEMPREG(temp_regs + 2), temp_regs + 3));
		PUSH_INS(INS3(OP_CODE_STORE_HEAP, TEMPREG(temp_regs), TEMPREG(temp_regs + 1), TEMPREG(temp_regs + 2)));
		break;
	case AST_VALUE_BINARY_OP: {
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.binary_op->lhs, TEMPREG(temp_regs), temp_regs + 1));
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.binary_op->rhs, TEMPREG(temp_regs + 1), temp_regs + 2));
		if (value->data.binary_op->lhs.type.type != value->type.type) {
			PUSH_INS(INS2(OP_CODE_LONG_TO_FLOAT, TEMPREG(temp_regs), TEMPREG(temp_regs)));
		}
		else if (value->data.binary_op->rhs.type.type != value->type.type) {
			PUSH_INS(INS2(OP_CODE_LONG_TO_FLOAT, TEMPREG(temp_regs + 1), TEMPREG(temp_regs + 1)));
		}
		if (value->data.binary_op->operator == TOK_EQUALS || value->data.binary_op->operator == TOK_NOT_EQUAL) {
			PUSH_INS(INS3(OP_CODE_BOOL_EQUAL + (value->type.type - TYPE_PRIMATIVE_BOOL), TEMPREG(temp_regs), TEMPREG(temp_regs + 1), out_reg));
			if (value->data.binary_op->operator == TOK_NOT_EQUAL)
				PUSH_INS(INS2(OP_CODE_NOT, out_reg, out_reg));
		}
		else if (value->data.binary_op->operator == TOK_AND)
			PUSH_INS(INS3(OP_CODE_AND, TEMPREG(temp_regs), TEMPREG(temp_regs + 1), out_reg))
		else {
			if (value->type.type == TYPE_PRIMATIVE_LONG)
				PUSH_INS(INS3(OP_CODE_LONG_MORE + (value->data.binary_op->operator - TOK_MORE), TEMPREG(temp_regs), TEMPREG(temp_regs + 1), out_reg))
			else if (value->type.type == TYPE_PRIMATIVE_FLOAT)
				PUSH_INS(INS3(OP_CODE_FLOAT_MORE + (value->data.binary_op->operator - TOK_MORE), TEMPREG(temp_regs), TEMPREG(temp_regs + 1), out_reg))
			else
				PANIC(compiler, ERROR_UNEXPECTED_TYPE);
		}
		break;
	}
	case AST_VALUE_UNARY_OP: {
		ESCAPE_ON_NULL(compile_ast_value(compiler, ins_builder, &value->data.unary_op->operand, out_reg, temp_regs));
		if (value->data.unary_op->operator == TOK_NOT)
			PUSH_INS(INS2(OP_CODE_NOT, out_reg, out_reg))
		else {
			if (value->type.type == TOK_TYPECHECK_LONG)
				PUSH_INS(INS2(OP_CODE_LONG_NEGATE, out_reg, out_reg))
			else
				PUSH_INS(INS2(OP_CODE_FLOAT_NEGATE, out_reg, out_reg))
		}
		break;
	}
	}
	return 1;
}

const int compile(compiler_t* compiler, machine_t* machine) {
	PANIC_ON_NULL(init_machine(machine, 10000, 1000, 1000), compiler, ERROR_OUT_OF_MEMORY);

	uint64_t initial_offset = compiler->ast.global_registers;
	compile_ast_code_block(compiler, machine, &compiler->ast.exec_block, &initial_offset);

	ins_builder_t ins_builder;
	PANIC_ON_NULL(init_ins_builder(&ins_builder), compiler, ERROR_OUT_OF_MEMORY);

	
}