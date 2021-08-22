#pragma once

#ifndef TYPE_H
#define TYPE_H

#include <stdint.h>

#define TYPE_MAX_SUBTYPES 100

typedef struct typecheck_type typecheck_type_t;

typedef struct typecheck_type {
	enum typecheck_type_type {
		TYPE_PRIMATIVE_BOOL,
		TYPE_PRIMATIVE_CHAR,
		TYPE_PRIMATIVE_LONG,
		TYPE_PRIMATIVE_FLOAT,
		TYPE_SUPER_ARRAY,
		TYPE_SUPER_PROC,
		TYPE_AUTO
	} type;

	typecheck_type_t* sub_types;
	uint8_t sub_type_count;
} typecheck_type_t;

#define DECL_PRIM_TYPE(TYPE) (typecheck_type_t){ .type = TYPE, .sub_types = NULL, .sub_type_count = 0};

const int init_typecheck_type(typecheck_type_t* typecheck_type, const int has_sub_types);
void free_typecheck_type(typecheck_type_t* typecheck_type);
const int copy_typecheck_type(typecheck_type_t* dest, typecheck_type_t src);

const int type_decl_sub_type(typecheck_type_t* super_type, typecheck_type_t sub_type);
const int typecheck_type_compatible(typecheck_type_t target_type, typecheck_type_t match_type);

#endif // !TYPE