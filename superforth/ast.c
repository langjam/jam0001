#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "error.h"
#include "ast.h"

#define READ_TOK ESCAPE_ON_NULL(scanner_read_tok(&ast->scanner))
#define MATCH_TOK(TOK) if(ast->scanner.last_tok.type != TOK) PANIC(ast, ERROR_UNEXPECTED_TOK);

static const int op_precs[] = {
	3, 3, 4, 4, 4, 5,
	1, 1, 1, 1, 1, 1, 1,
	2, 2
};

static const int parse_value(ast_t* ast, ast_value_t* value);
static const int parse_expression(ast_t* ast, ast_value_t* value, int min_prec);
static const int parse_code_block(ast_t * ast, ast_code_block_t * code_block, uint64_t * current_reg, uint64_t * register_limit, int encapsulated);

static void free_ast_value(ast_value_t * value);
static void free_ast_code_block(ast_code_block_t * code_block);

static void ast_var_cache_new_frame(ast_t* ast, typecheck_type_t* return_type, int access_previous_frame) {
	uint8_t stack_top = ++ast->var_cache.stack_top;
	ast->var_cache.pop_bounds[stack_top] = ast->var_cache.current_entry;
	if (access_previous_frame) {
		ast->var_cache.search_bounds[stack_top] = ast->var_cache.search_bounds[stack_top - 1];
		if (ast->var_cache.return_type_count)
			ast->var_cache.return_types[ast->var_cache.return_type_count] = ast->var_cache.return_types[ast->var_cache.return_type_count - 1];
		else
			ast->var_cache.return_types[ast->var_cache.return_type_count] = NULL;
		ast->var_cache.return_type_count++;
	}
	else {
		ast->var_cache.search_bounds[stack_top] = ast->var_cache.current_entry;
		ast->var_cache.return_types[ast->var_cache.return_type_count++] = return_type;
	}
}

static void ast_var_cache_close_frame(ast_t* ast) {
	ast->var_cache.stack_top--;
	ast->var_cache.return_type_count--;
}

static const int ast_var_cache_decl_var(ast_t* ast, uint64_t id_hash, ast_var_info_t var_info, int global) {
	if (global) {
		for (uint8_t i = 0; i < ast->var_cache.global_entry_count; i++)
			if (ast->var_cache.global_entries[i].id_hash == id_hash)
				return 0;
		ast->var_cache.global_entries[ast->var_cache.global_entry_count++] = (ast_var_cache_entry_t){
			.id_hash = id_hash,
			.var_info = var_info
		};
	}
	else {
		for (int16_t i = ast->var_cache.current_entry - 1; i >= ast->var_cache.search_bounds[ast->var_cache.stack_top]; i--)
			if (ast->var_cache.entries[i].id_hash == id_hash)
				return 0;
		ast->var_cache.entries[ast->var_cache.current_entry++] = (ast_var_cache_entry_t){
			.id_hash = id_hash,
			.var_info = var_info
		};
	}
	return 1;
}

static const int ast_var_cache_find_info(ast_t* ast, uint64_t id_hash, ast_var_info_t* info_out) {
	for (int16_t i = ast->var_cache.current_entry - 1; i >= ast->var_cache.search_bounds[ast->var_cache.stack_top]; i--)
		if (ast->var_cache.entries[i].id_hash == id_hash) {
			*info_out = ast->var_cache.entries[i].var_info;
			return 1;
		}
	for (uint8_t i = 0; i < ast->var_cache.global_entry_count; i++)
		if (ast->var_cache.global_entries[i].id_hash = id_hash) {
			*info_out = ast->var_cache.global_entries[i].var_info;
			return 1;
		}
	return 0;
}

static const int init_ast_code_block(ast_code_block_t* code_block) {
	code_block->instruction_count = 0;
	ESCAPE_ON_NULL(code_block->instructions = malloc((code_block->allocated_instructions = 16) * sizeof(ast_top_level_t)));
	return 1;
}

static void free_ast_array_lit(ast_array_literal_t* array_literal) {
	for (uint32_t i = 0; i < array_literal->element_count; i++)
		free_ast_value(&array_literal->elements[i]);
	free(array_literal->elements);
	free_typecheck_type(&array_literal->elem_type);
}

static void free_ast_call_proc(ast_call_proc_t* call_proc) {
	for (uint_fast8_t i = 0; i < call_proc->argument_count; i++)
		free_ast_value(&call_proc->arguments[i]);
	free_ast_value(&call_proc->procedure);
}

static void free_ast_value(ast_value_t* value) {
	free_typecheck_type(&value->type);
	switch (value->value_type)
	{
	case AST_VALUE_ALLOC_ARRAY:
		free_typecheck_type(&value->data.alloc_array->elem_type);
		free(value->data.alloc_array);
		break;
	case AST_VALUE_ARRAY_LITERAL:
		free_ast_array_lit(&value->data.array_literal);
		break;
	case AST_VALUE_PROC:
		free_typecheck_type(&value->data.procedure->return_type);
		free_ast_code_block(&value->data.procedure->exec_block);
		free(value->data.procedure);
		break;
	case AST_VALUE_GET_INDEX:
		free_ast_value(&value->data.get_index->array);
		free_ast_value(&value->data.get_index->index);
		free(value->data.procedure);
		break;
	case AST_VALUE_SET_VAR:
		free_ast_value(&value->data.set_var->set_value);
		free(value->data.set_var);
		break;
	case AST_VALUE_SET_INDEX:
		free_ast_value(&value->data.set_index->array);
		free_ast_value(&value->data.set_index->index);
		free_ast_value(&value->data.set_index->value);
		free(value->data.set_index);
		break;
	case AST_VALUE_BINARY_OP:
		free_ast_value(&value->data.binary_op->lhs);
		free_ast_value(&value->data.binary_op->rhs);
		free(value->data.binary_op);
		break;
	case AST_VALUE_UNARY_OP:
		free_ast_value(&value->data.unary_op->operand);
		free(value->data.unary_op);
	case AST_VALUE_PROC_CALL:
		free_ast_call_proc(value->data.proc_call);
		free(value->data.proc_call);
		break;
	}
}

static void free_ast_conditional(ast_cond_t* conditional) {
	if (conditional->has_cond_val)
		free_ast_value(&conditional->cond_val);
	free_ast_code_block(&conditional->exec_block);
	if (conditional->next_if_true && conditional->next_if_true != conditional)
		free_ast_conditional(conditional->next_if_true);
	if (conditional->next_if_false && conditional->next_if_false != conditional)
		free_ast_conditional(conditional->next_if_false);
	free(conditional);
}

static void free_ast_top_lvl(ast_top_level_t top_level_ins) {
	switch (top_level_ins.type) {
	case AST_TOP_LEVEL_RETURN:
	case AST_TOP_LEVEL_VALUE:
		free_ast_value(&top_level_ins.data.value);
		break;
	case AST_TOP_LEVEL_DECL_VAR:
		free_typecheck_type(&top_level_ins.data.var_decl.var_info.type);
		free_ast_value(&top_level_ins.data.var_decl.set_value);
		break;
	case AST_TOP_LEVEL_COND:
		free_ast_conditional(top_level_ins.data.conditional);
		break;
	}
}

static void free_ast_code_block(ast_code_block_t* code_block) {
	for (uint_fast32_t i = 0; i < code_block->instruction_count; i++)
		free_ast_top_lvl(code_block->instructions[i]);
	free(code_block->instructions);
}

static const int ast_code_block_append_ins(ast_code_block_t* code_block, ast_top_level_t instruction) {
	if (code_block->instruction_count == code_block->allocated_instructions) {
		code_block->allocated_instructions *= 2;
		ast_top_level_t* new_instructions = realloc(code_block->instructions, code_block->allocated_instructions * sizeof(ast_top_level_t));
		ESCAPE_ON_NULL(new_instructions);
		code_block->instructions = new_instructions;
	}
	code_block->instructions[code_block->instruction_count++] = instruction;
	return 1;
}

static const int parse_type_decl(ast_t* ast, typecheck_type_t* typecheck_type, int allow_auto) {
	enum typecheck_type_type type;
	switch (ast->scanner.last_tok.type)
	{
	case TOK_TYPECHECK_BOOL:
		type = TYPE_PRIMATIVE_BOOL;
		break;
	case TOK_TYPECHECK_CHAR:
		type = TYPE_PRIMATIVE_CHAR;
		break;
	case TOK_TYPECHECK_LONG:
		type = TYPE_PRIMATIVE_LONG;
		break;
	case TOK_TYPECHECK_FLOAT:
		type = TYPE_PRIMATIVE_FLOAT;
		break;
	case TOK_TYPECHECK_ARRAY:
		type = TYPE_SUPER_ARRAY;
		break;
	case TOK_TYPECHECK_PROC:
		type = TYPE_SUPER_PROC;
		break;
	case TOK_AUTO:
		if (allow_auto)
			type = TYPE_AUTO;
		else
			PANIC(ast, ERROR_NO_AUTO_TYPING);
		break;
	default:
		PANIC(ast, ERROR_UNEXPECTED_TOK);
	}

	READ_TOK;
	if (ast->scanner.last_tok.type == TOK_LESS) {
		typecheck_type->type = type;
		typecheck_type_t sub_types[TYPE_MAX_SUBTYPES];
		uint8_t found_sub_types = 0;
		do {
			if (found_sub_types == TYPE_MAX_SUBTYPES)
				PANIC(ast, ERROR_TO_MANY_SUB_TYPES);
			READ_TOK;
			ESCAPE_ON_NULL(parse_type_decl(ast, &sub_types[found_sub_types++], 0));
		} while (ast->scanner.last_tok.type == TOK_COMMA);
		MATCH_TOK(TOK_MORE);
		READ_TOK;

		init_typecheck_type(typecheck_type, 1);
		for (uint_fast8_t i = 0; i < found_sub_types; i++)
			PANIC_ON_NULL(type_decl_sub_type(typecheck_type, sub_types[i]), ast, ERROR_TO_MANY_SUB_TYPES);

		if (type == TYPE_SUPER_ARRAY && found_sub_types != 1)
			PANIC(ast, ERROR_TO_MANY_SUB_TYPES);
	}
	else if (type == TYPE_SUPER_ARRAY || type == TYPE_SUPER_PROC)
		PANIC(ast, ERROR_EXPECTED_SUB_TYPES)
	else {
		init_typecheck_type(typecheck_type, 0);
		typecheck_type->type = type;
	}
	return 1;
}

static const int parse_id(ast_t* ast, ast_id_t* id) {
	MATCH_TOK(TOK_IDENTIFIER);
	id->c_str = ast->scanner.last_tok.str;
	id->length = ast->scanner.last_tok.length;
	id->hash = hash_s(id->c_str, id->length);
	READ_TOK;
	return 1;
}

static const int parse_var_decl(ast_t* ast, ast_decl_var_t* var_decl, uint64_t* current_reg, uint64_t* register_limit) {
	if (ast->scanner.last_tok.type == TOK_GLOBAL) {
		var_decl->global_flag = 1;
		READ_TOK;
	}
	else
		var_decl->global_flag = 0;
	ESCAPE_ON_NULL(parse_type_decl(ast, &var_decl->var_info.type, 1));
	ESCAPE_ON_NULL(parse_id(ast, &var_decl->id));

	MATCH_TOK(TOK_SET);
	READ_TOK;

	ESCAPE_ON_NULL(parse_expression(ast, &var_decl->set_value, 0));

	if (var_decl->var_info.type.type == TYPE_AUTO) {
		free_typecheck_type(&var_decl->var_info.type);
		PANIC_ON_NULL(copy_typecheck_type(&var_decl->var_info.type, var_decl->set_value.type), ast, ERROR_OUT_OF_MEMORY);
	}
	else if (!typecheck_type_compatible(var_decl->var_info.type, var_decl->set_value.type))
		PANIC(ast, ERROR_UNEXPECTED_TYPE);
	var_decl->var_info.alloced_reg.offset_flag = !var_decl->global_flag;
	if (var_decl->global_flag)
		var_decl->var_info.alloced_reg.index = ast->global_registers++;
	else {
		var_decl->var_info.alloced_reg.index = *current_reg;
		(*current_reg)++;
		(*register_limit)++;
	}
	ast_var_cache_decl_var(ast, var_decl->id.hash, var_decl->var_info, var_decl->global_flag);
	return 1;
}

static const int parse_cond_expr(ast_t* ast, ast_value_t* cond_expr) {
	MATCH_TOK(TOK_OPEN_PAREN);
	READ_TOK;
	ESCAPE_ON_NULL(parse_expression(ast, cond_expr, 0));
	if (cond_expr->type.type != TOK_TYPECHECK_BOOL)
		PANIC(ast, ERROR_UNEXPECTED_TYPE);
	MATCH_TOK(TOK_CLOSE_PAREN);
	READ_TOK;
	return 1;
}

static const int parse_conditional(ast_t* ast, ast_cond_t* conditional, uint64_t* current_reg, uint64_t* register_limit) {
	uint64_t old_reg = *current_reg;
	if (ast->scanner.last_tok.type == TOK_IF) {
		READ_TOK;
		ESCAPE_ON_NULL(parse_cond_expr(ast, &conditional->cond_val));

		ast_var_cache_new_frame(ast, NULL, 1);

		uint64_t block_limit;

		ESCAPE_ON_NULL(parse_code_block(ast, &conditional->exec_block, current_reg, register_limit, 1));
		(*current_reg) = old_reg;
		ast_var_cache_close_frame(ast);

		conditional->next_if_true = NULL;
		conditional->has_cond_val = 1;
		if (ast->scanner.last_tok.type == TOK_ELSE)
		{
			READ_TOK;
			if (ast->scanner.last_tok.type == TOK_IF) {
				block_limit = *register_limit;
				PANIC_ON_NULL(conditional->next_if_false = malloc(sizeof(ast_cond_t)), ast, ERROR_OUT_OF_MEMORY);
				ESCAPE_ON_NULL(parse_conditional(ast, conditional->next_if_false, current_reg, &block_limit));
				*register_limit = max(block_limit, *register_limit);
				conditional = conditional->next_if_false;
			}
			else {
				PANIC_ON_NULL(conditional->next_if_false = malloc(sizeof(ast_cond_t)), ast, ERROR_OUT_OF_MEMORY);
				conditional = conditional->next_if_false;
				conditional->next_if_true = NULL;
				conditional->next_if_false = NULL;
				conditional->has_cond_val = 0;

				block_limit = *register_limit;
				ast_var_cache_new_frame(ast, NULL, 1);
				ESCAPE_ON_NULL(parse_code_block(ast, &conditional->exec_block, current_reg, &block_limit, 1));
				*register_limit = max(block_limit, *register_limit);
				(*current_reg) = old_reg;
				ast_var_cache_close_frame(ast);
			}
		}
		else
			conditional->next_if_false = NULL;
	}
	else if (ast->scanner.last_tok.type == TOK_WHILE) {
		READ_TOK;
		conditional->next_if_true = conditional;
		conditional->next_if_false = NULL;
		conditional->has_cond_val = 1;

		ESCAPE_ON_NULL(parse_cond_expr(ast, &conditional->cond_val));

		ast_var_cache_new_frame(ast, NULL, 1);
		ESCAPE_ON_NULL(parse_code_block(ast, &conditional->exec_block, current_reg, register_limit, 1));
		(*current_reg) = old_reg;
		ast_var_cache_close_frame(ast);
	}
	else
		PANIC(ast, ERROR_UNEXPECTED_TOK);
	return 1;
}

static const int parse_array_lit(ast_t* ast, ast_value_t* value) {
	value->value_type = AST_VALUE_ARRAY_LITERAL;
	MATCH_TOK(TOK_OPEN_BRACKET);

	uint32_t allocated_values = 64;
	PANIC_ON_NULL(value->data.array_literal.elements = malloc(allocated_values * sizeof(ast_value_t)), ast, ERROR_OUT_OF_MEMORY);
	value->data.array_literal.element_count = 0;

	int type_flag = 0;
	do {
		READ_TOK;
		if (value->data.array_literal.element_count == allocated_values) {
			allocated_values *= 2;
			ast_value_t* new_elements = realloc(value->data.array_literal.elements, allocated_values * sizeof(ast_value_t));
			PANIC_ON_NULL(new_elements, ast, ERROR_OUT_OF_MEMORY);
			value->data.array_literal.elements = new_elements;
		}
		ast_value_t* elem = &value->data.array_literal.elements[value->data.array_literal.element_count++];
		ESCAPE_ON_NULL(parse_expression(ast, elem, 0));

		if (type_flag) {
			if (!typecheck_type_compatible(value->data.array_literal.elem_type, elem->type))
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
		}
		else {
			PANIC_ON_NULL(copy_typecheck_type(&value->data.array_literal.elem_type, elem->type), ast, ERROR_OUT_OF_MEMORY);
			type_flag = 1;
		}
	} while (ast->scanner.last_tok.type == TOK_COMMA);

	MATCH_TOK(TOK_CLOSE_BRACKET);
	READ_TOK;

	value->type = (typecheck_type_t){
		.type = TYPE_SUPER_ARRAY,
		.sub_type_count = 1,
	};
	PANIC_ON_NULL(value->type.sub_types = malloc(sizeof(typecheck_type_t)), ast, ERROR_OUT_OF_MEMORY);
	PANIC_ON_NULL(copy_typecheck_type(&value->type.sub_types[0], value->data.array_literal.elem_type), ast, ERROR_OUT_OF_MEMORY);

	return 1;
}

static const int parse_prim_value(ast_t* ast, ast_value_t* value) {
	switch (ast->scanner.last_tok.type)
	{
	case TOK_NUMERICAL: {
		for (uint_fast32_t i = 0; i < ast->scanner.last_tok.length; i++)
			if (ast->scanner.last_tok.str[i] == '.' || ast->scanner.last_tok.str[i] == 'f') { //is a float
				value->data.float_int = strtod(ast->scanner.last_tok.str, NULL);
				value->value_type = AST_VALUE_FLOAT;
				value->type = (typecheck_type_t){
					.type = TYPE_PRIMATIVE_FLOAT,
					.sub_type_count = 0,
					.sub_types = NULL
				};
				READ_TOK;
				return 1;
			}
		value->value_type = AST_VALUE_LONG;
		value->data.long_int = strtol(ast->scanner.last_tok.str, NULL, 10);
		value->type = (typecheck_type_t){
			.type = TYPE_PRIMATIVE_LONG,
			.sub_type_count = 0,
			.sub_types = NULL
		};
		READ_TOK;
		return 1;
	}
	case TOK_CHAR: {
		value->value_type = AST_VALUE_CHAR;
		value->data.character = ast->scanner.last_tok.str[0];
		value->type = (typecheck_type_t){
			.type = TYPE_PRIMATIVE_CHAR,
			.sub_type_count = 0,
			.sub_types = NULL
		};
		READ_TOK;
		return 1;
	}
	case TOK_STRING: {
		value->value_type = AST_VALUE_ARRAY_LITERAL;
		value->type = (typecheck_type_t){
			.type = TYPE_SUPER_ARRAY,
			.sub_type_count = 1,
		};
		PANIC_ON_NULL(value->type.sub_types = malloc(sizeof(typecheck_type_t)), ast, ERROR_OUT_OF_MEMORY);
		value->data.array_literal.elem_type = value->type.sub_types[0] = (typecheck_type_t){
			.type = TYPE_PRIMATIVE_CHAR,
			.sub_type_count = 0,
			.sub_types = NULL
		};
		value->data.array_literal.element_count = ast->scanner.last_tok.length;
		PANIC_ON_NULL(value->data.array_literal.elements = malloc(ast->scanner.last_tok.length * sizeof(ast_value_t)),ast, ERROR_OUT_OF_MEMORY);
		for (uint_fast32_t i = 0; i < ast->scanner.last_tok.length; i++)
			value->data.array_literal.elements[i] = (ast_value_t){
				.value_type = AST_VALUE_CHAR,
				.data.character = ast->scanner.last_tok.str[i],
				.type = value->data.array_literal.elem_type
		};
		READ_TOK;
		return 1;
	}
	case TOK_TRUE:
	case TOK_FALSE: {
		value->value_type = AST_VALUE_BOOL;
		value->type = (typecheck_type_t){
			.type = TYPE_PRIMATIVE_BOOL,
			.sub_type_count = 0,
			.sub_types = NULL
		};
		value->data.bool_flag = ast->scanner.last_tok.type == TOK_TRUE;
		READ_TOK;
		return 1; 
	}
	case TOK_OPEN_BRACKET:
		return parse_array_lit(ast, value);
	case TOK_NEW: {
		value->value_type = AST_VALUE_ALLOC_ARRAY;
		READ_TOK;
		PANIC_ON_NULL(value->data.alloc_array = malloc(sizeof(ast_alloc_t)), ast, ERROR_OUT_OF_MEMORY);
		ESCAPE_ON_NULL(parse_type_decl(ast, &value->data.alloc_array->elem_type, 0));
		PANIC_ON_NULL(init_typecheck_type(&value->type, 1), ast, ERROR_OUT_OF_MEMORY);
		PANIC_ON_NULL(copy_typecheck_type(&value->type.sub_types[0], value->data.alloc_array->elem_type), ast, ERROR_OUT_OF_MEMORY);
		value->type.sub_type_count = 1;
		value->type.type = TYPE_SUPER_ARRAY;

		MATCH_TOK(TOK_OPEN_BRACKET);
		READ_TOK;
		
		ESCAPE_ON_NULL(parse_value(ast, &value->data.alloc_array->size));
		if (value->data.alloc_array->size.type.type != TYPE_PRIMATIVE_LONG)
			PANIC(ast, ERROR_UNEXPECTED_TYPE);

		MATCH_TOK(TOK_CLOSE_BRACKET);
		READ_TOK;
		return 1;
	}
	}
	PANIC(ast, ERROR_UNEXPECTED_TOK);
}

static const int parse_proc_lit(ast_t* ast, ast_value_t* value) {
	value->value_type = AST_VALUE_PROC;
	PANIC_ON_NULL(value->data.procedure = malloc(sizeof(ast_proc_t)), ast, ERROR_OUT_OF_MEMORY);
	value->data.procedure->param_count = 0;

	ESCAPE_ON_NULL(parse_type_decl(ast, &value->data.procedure->return_type, 1));
	
	MATCH_TOK(TOK_OPEN_PAREN);

	uint64_t current_reg = 0;
	value->data.procedure->exec_block.register_limit = 0;
	ast_var_cache_new_frame(ast, NULL, 0);

	do {
		READ_TOK;
		if (ast->scanner.last_tok.type == TOK_CLOSE_PAREN)
			break;
		if (value->data.procedure->param_count == TYPE_MAX_SUBTYPES - 1)
			PANIC(ast, ERROR_TO_MANY_SUB_TYPES);
		struct ast_proc_param* param = &value->data.procedure->params[value->data.procedure->param_count++];
		ESCAPE_ON_NULL(parse_type_decl(ast, &param->var_info.type, 0));
		ESCAPE_ON_NULL(parse_id(ast, &param->id));

		param->var_info.alloced_reg.index = current_reg++;
		param->var_info.alloced_reg.offset_flag = 1;
		ast_var_cache_decl_var(ast, param->id.hash, param->var_info, 0);
		value->data.procedure->exec_block.register_limit++;
	} while (ast->scanner.last_tok.type == TOK_COMMA);
	MATCH_TOK(TOK_CLOSE_PAREN);
	READ_TOK;

	value->type = (typecheck_type_t){
		.type = TYPE_SUPER_PROC,
		.sub_type_count = 1 + value->data.procedure->param_count
	};

	PANIC_ON_NULL(value->type.sub_types = malloc(value->type.sub_type_count * sizeof(typecheck_type_t)), ast, ERROR_OUT_OF_MEMORY);
	PANIC_ON_NULL(copy_typecheck_type(&value->type.sub_types[0], value->data.procedure->return_type), ast, ERROR_OUT_OF_MEMORY);
	for (uint_fast8_t i = 0; i < value->data.procedure->param_count; i++)
		PANIC_ON_NULL(copy_typecheck_type(&value->type.sub_types[i + 1], value->data.procedure->params[i].var_info.type), ast, ERROR_OUT_OF_MEMORY);
	ast->var_cache.return_types[ast->var_cache.return_type_count - 1] = &value->type.sub_types[0];

	PANIC_ON_NULL(ast_var_cache_decl_var(ast, 7572967076558961, (ast_var_info_t) { .alloced_reg = (ast_register_t){ .index = current_reg++, .offset_flag = 1 }, .type = value->type }, 0), ast, ERROR_OUT_OF_MEMORY);

	ESCAPE_ON_NULL(parse_code_block(ast, &value->data.procedure->exec_block, &current_reg, &value->data.procedure->exec_block.register_limit, 1));
	free_typecheck_type(&value->data.procedure->return_type);
	PANIC_ON_NULL(copy_typecheck_type(&value->data.procedure->return_type, value->type.sub_types[0]), ast, ERROR_OUT_OF_MEMORY);

	ast_var_cache_close_frame(ast);

	return 1;
}

static const int parse_code_block(ast_t* ast, ast_code_block_t* code_block, uint64_t* current_reg, uint64_t* register_limit, int encapsulated) {
	init_ast_code_block(code_block);

	if (encapsulated) {
		MATCH_TOK(TOK_OPEN_BRACE);
		READ_TOK;
	}

	do {
		ast_top_level_t top_level_ins;
		switch (ast->scanner.last_tok.type)
		{
		case TOK_AUTO:
		case TOK_GLOBAL:
		case TOK_TYPECHECK_ARRAY:
		case TOK_TYPECHECK_BOOL:
		case TOK_TYPECHECK_CHAR:
		case TOK_TYPECHECK_FLOAT:
		case TOK_TYPECHECK_LONG:
		case TOK_TYPECHECK_PROC: {
			ESCAPE_ON_NULL(parse_var_decl(ast, &top_level_ins.data.var_decl, current_reg, register_limit));
			top_level_ins.type = AST_TOP_LEVEL_DECL_VAR;
			break;
		}
		case TOK_IDENTIFIER: {
			ESCAPE_ON_NULL(parse_value(ast, &top_level_ins.data.value));
			if (top_level_ins.data.value.value_type != AST_VALUE_SET_INDEX && top_level_ins.data.value.value_type != AST_VALUE_SET_VAR &&
				top_level_ins.data.value.value_type != AST_VALUE_PROC_CALL)
				PANIC(ast, ERROR_UNEXPECTED_TOK);
			top_level_ins.type = AST_TOP_LEVEL_VALUE;
			break;
		}
		case TOK_RETURN: {
			READ_TOK;
			top_level_ins.type = AST_TOP_LEVEL_RETURN;
			ESCAPE_ON_NULL(parse_expression(ast, &top_level_ins.data.value, 0));

			typecheck_type_t* return_type = ast->var_cache.return_types[ast->var_cache.return_type_count - 1];
			if (return_type->type == TYPE_AUTO) {
				free_typecheck_type(return_type);
				copy_typecheck_type(return_type, top_level_ins.data.value.type);
			}
			else if (!typecheck_type_compatible(*return_type, top_level_ins.data.value.type))
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
			break;
		}
		case TOK_IF:
		case TOK_WHILE: {
			top_level_ins.type = AST_TOP_LEVEL_COND;
			PANIC_ON_NULL(top_level_ins.data.conditional = malloc(sizeof(ast_cond_t)), ast, ERROR_OUT_OF_MEMORY);
			ESCAPE_ON_NULL(parse_conditional(ast, top_level_ins.data.conditional, current_reg, register_limit)); 
			break;
		}
		default:
			PANIC(ast, ERROR_UNEXPECTED_TOK);
		}
		PANIC_ON_NULL(ast_code_block_append_ins(code_block, top_level_ins), ast, ERROR_OUT_OF_MEMORY);
	} while (ast->scanner.last_tok.type != TOK_EOF && ast->scanner.last_tok.type != TOK_CLOSE_BRACE);
	if (encapsulated) {
		MATCH_TOK(TOK_CLOSE_BRACE);
		READ_TOK;
	}
	return 1;
}

static const int parse_value(ast_t* ast, ast_value_t* value) {
	switch (ast->scanner.last_tok.type)
	{
	case TOK_AUTO:
	case TOK_GLOBAL:
	case TOK_TYPECHECK_ARRAY:
	case TOK_TYPECHECK_BOOL:
	case TOK_TYPECHECK_CHAR:
	case TOK_TYPECHECK_FLOAT:
	case TOK_TYPECHECK_LONG:
	case TOK_TYPECHECK_PROC: {
		ESCAPE_ON_NULL(parse_proc_lit(ast, value)); 
		break;
	}
	case TOK_NUMERICAL:
	case TOK_STRING:
	case TOK_CHAR:
	case TOK_TRUE:
	case TOK_FALSE:
	case TOK_OPEN_BRACKET:
	case TOK_NEW:
		return parse_prim_value(ast, value);
	case TOK_NOT:
	case TOK_SUBTRACT: {
		value->value_type = AST_VALUE_UNARY_OP;
		ESCAPE_ON_NULL(parse_value(ast, &value->data.unary_op->operand));
		if ((ast->scanner.last_tok.type == TOK_NOT && value->data.unary_op->operand.type.type != TYPE_PRIMATIVE_BOOL) ||
			(ast->scanner.last_tok.type == TOK_SUBTRACT && value->data.unary_op->operand.type.type != TYPE_PRIMATIVE_FLOAT && value->data.unary_op->operand.type.type != TYPE_PRIMATIVE_LONG))
			PANIC(ast, ERROR_UNEXPECTED_TYPE);
		value->data.unary_op->operator = ast->scanner.last_tok.type;
		copy_typecheck_type(&value->type, value->data.unary_op->operand.type);
		break;
	}
	case TOK_IDENTIFIER: {
		ast_id_t id;
		parse_id(ast, &id);

		ast_var_info_t var_info;
		if (!ast_var_cache_find_info(ast, id.hash, &var_info))
			PANIC(ast, ERROR_UNDECLARED_VAR);

		if (ast->scanner.last_tok.type == TOK_SET) {
			READ_TOK;
			value->value_type = AST_VALUE_SET_VAR;
			PANIC_ON_NULL(value->data.set_var = malloc(sizeof(ast_set_var_t)), ast, ERROR_OUT_OF_MEMORY);
			value->data.set_var->id = id;

			ESCAPE_ON_NULL(parse_expression(ast, &value->data.set_var->set_value, 0));

			if(!typecheck_type_compatible(var_info.type, value->data.set_var->set_value.type))
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
			copy_typecheck_type(&value->type, var_info.type);
			value->alloced_reg = var_info.alloced_reg;
		}
		else {
			value->value_type = AST_VALUE_VAR;
			value->data.variable = id;
			value->alloced_reg = var_info.alloced_reg;
			copy_typecheck_type(&value->type, var_info.type);
		}
		break;
	}
	default:
		PANIC(ast, ERROR_UNEXPECTED_TOK);
	}
	while (ast->scanner.last_tok.type == TOK_OPEN_BRACKET || ast->scanner.last_tok.type == TOK_OPEN_PAREN) {
		switch (ast->scanner.last_tok.type)
		{
		case TOK_OPEN_BRACKET: {
			READ_TOK;
			ast_value_t index_value;
			ESCAPE_ON_NULL(parse_expression(ast, &index_value, 0));
			if (index_value.type.type != TYPE_PRIMATIVE_LONG || value->type.type != TYPE_SUPER_ARRAY)
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
			ast_value_t array_value = *value;

			MATCH_TOK(TOK_CLOSE_BRACKET);
			READ_TOK;
			
			if (ast->scanner.last_tok.type == TOK_SET) {
				READ_TOK;

				ast_value_t set_val;
				ESCAPE_ON_NULL(parse_value(ast, &set_val));

				if (!typecheck_type_compatible(array_value.type.sub_types[0], set_val.type))
					PANIC(ast, ERROR_UNEXPECTED_TYPE);

				value->value_type = AST_VALUE_SET_INDEX;
				copy_typecheck_type(&value->type, array_value.type.sub_types[0]);

				PANIC_ON_NULL(value->data.set_index = malloc(sizeof(ast_set_index_t)), ast, ERROR_OUT_OF_MEMORY);
				*value->data.set_index = (ast_set_index_t){
					.array = array_value,
					.index = index_value,
					.value = set_val
				};
			}
			else {
				value->value_type = AST_VALUE_GET_INDEX;
				copy_typecheck_type(&value->type, array_value.type.sub_types[0]);

				PANIC_ON_NULL(value->data.get_index = malloc(sizeof(ast_get_index_t)), ast, ERROR_OUT_OF_MEMORY);
				*value->data.get_index = (ast_get_index_t){
					.array = array_value,
					.index = index_value
				};
			}
			continue;
		}
		case TOK_OPEN_PAREN: {
			if(value->type.type != TYPE_SUPER_PROC)
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
			ast_value_t proc_call_val = {
				.value_type = AST_VALUE_PROC_CALL,
			};
			copy_typecheck_type(&proc_call_val.type, value->type.sub_types[0]);
			PANIC_ON_NULL(proc_call_val.data.proc_call = malloc(sizeof(ast_call_proc_t)), ast, ERROR_OUT_OF_MEMORY);
			
			ast_call_proc_t* proc_call = proc_call_val.data.proc_call;
			proc_call->procedure = *value;
			proc_call->argument_count = 0;

			do {
				READ_TOK;
				if (ast->scanner.last_tok.type == TOK_CLOSE_PAREN)
					break;
				if (proc_call->argument_count == value->type.sub_type_count - 1)
					PANIC(ast, ERROR_UNEXPECTED_ARGUMENT_LENGTH);
				
				ESCAPE_ON_NULL(parse_expression(ast, &proc_call->arguments[proc_call->argument_count++], 0));
				if (!typecheck_type_compatible(value->type.sub_types[proc_call->argument_count], proc_call->arguments[proc_call->argument_count - 1].type))
					PANIC(ast, ERROR_UNEXPECTED_TYPE);
			} while (ast->scanner.last_tok.type == TOK_COMMA);
			
			if (proc_call->argument_count != value->type.sub_type_count - 1)
				PANIC(ast, ERROR_UNEXPECTED_ARGUMENT_LENGTH);
			
			MATCH_TOK(TOK_CLOSE_PAREN);
			READ_TOK;

			*value = proc_call_val;
			continue;
		}
		}
	}
	return 1;
}

static const int parse_expression(ast_t* ast, ast_value_t* value, int min_prec) {
	ast_value_t lhs;
	ESCAPE_ON_NULL(parse_value(ast, &lhs));

	while (ast->scanner.last_tok.type >= TOK_ADD && ast->scanner.last_tok.type <= TOK_OR && op_precs[ast->scanner.last_tok.type - TOK_ADD] > min_prec)
	{
		token_type_t op_tok = ast->scanner.last_tok.type;
		READ_TOK;

		ast_value_t rhs;
		ESCAPE_ON_NULL(parse_expression(ast, &rhs, op_precs[op_tok - TOK_ADD]));

		if (op_tok < TOK_EQUALS || op_tok > TOK_NOT_EQUAL) {
			if ((lhs.type.type != TYPE_PRIMATIVE_FLOAT && lhs.type.type != TYPE_PRIMATIVE_LONG) ||
				(rhs.type.type != TYPE_PRIMATIVE_FLOAT && rhs.type.type != TYPE_PRIMATIVE_LONG))
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
		}
		else if (op_tok == TOK_AND || op_tok == TOK_OR) {
			if (lhs.type.type != TYPE_PRIMATIVE_BOOL || rhs.type.type != TYPE_PRIMATIVE_BOOL)
				PANIC(ast, ERROR_UNEXPECTED_TYPE);
		}
		else if(lhs.type.type != TYPE_SUPER_PROC && lhs.type.type != TYPE_SUPER_ARRAY &&
			rhs.type.type != TYPE_SUPER_PROC && rhs.type.type != TYPE_SUPER_ARRAY && !typecheck_type_compatible(lhs.type, rhs.type))
			PANIC(ast, ERROR_UNEXPECTED_TYPE);

		ast_value_t bin_op = {
			.value_type = AST_VALUE_BINARY_OP,
		};
		PANIC_ON_NULL(bin_op.data.binary_op = malloc(sizeof(ast_binary_op_t)), ast, ERROR_OUT_OF_MEMORY);

		*bin_op.data.binary_op = (ast_binary_op_t){
			.lhs = lhs,
			.rhs = rhs,
			.operator = op_tok
		};

		if (op_tok < TOK_EQUALS) {
			bin_op.type = (typecheck_type_t){
				.type = max(lhs.type.type, rhs.type.type),
				.sub_type_count = 0,
				.sub_types = NULL
			};
		}
		else {
			bin_op.type = (typecheck_type_t){
				.type = TOK_TYPECHECK_BOOL,
				.sub_type_count = 0,
				.sub_types = NULL
			};
		}

		lhs = bin_op;
	}
	*value = lhs;
	return 1;
}

const int init_ast(ast_t* ast, const char* source) {
	ast->last_err = ERROR_NONE;
	ast->var_cache.stack_top = 0;
	ast->var_cache.current_entry = 0;
	ast->var_cache.global_entry_count = 0;
	ast->var_cache.return_type_count = 0;
	ast->global_registers = 0;
	ast->var_cache.search_bounds[0] = 0;
	ast->var_cache.pop_bounds[0] = 0;
	
	init_scanner(&ast->scanner, source, strlen(source));
	uint64_t current_reg = 0;
	ast->exec_block.register_limit = 0;
	ESCAPE_ON_NULL(parse_code_block(ast, &ast->exec_block, &current_reg, &ast->exec_block.register_limit, 0));
	
	return 1;
}

void free_ast(ast_t* ast) {
	free_ast_code_block(&ast->exec_block);
}