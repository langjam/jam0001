#include <stdlib.h>
#include "error.h"
#include "type.h"

const int init_typecheck_type(typecheck_type_t* typecheck_type, const int has_sub_types) {
	if (has_sub_types)
		ESCAPE_ON_NULL(typecheck_type->sub_types = malloc(TYPE_MAX_SUBTYPES * sizeof(typecheck_type_t)))
	else
		typecheck_type->sub_types = NULL;
	typecheck_type->sub_type_count = 0;
	return 1;
}

void free_typecheck_type(typecheck_type_t* typecheck_type) {
	for (uint_fast8_t i = 0; i < typecheck_type->sub_type_count; i++)
		free_typecheck_type(&typecheck_type->sub_types[i]);
	if (typecheck_type->sub_types)
		free(typecheck_type->sub_types);
}

const int copy_typecheck_type(typecheck_type_t* dest, typecheck_type_t src) {
	dest->type = src.type;
	dest->sub_type_count = src.sub_type_count;
	if (src.sub_type_count) {
		ESCAPE_ON_NULL(dest->sub_types = malloc(src.sub_type_count * sizeof(typecheck_type_t)));
		for (uint_fast8_t i = 0; i < src.sub_type_count; i++)
			copy_typecheck_type(&dest->sub_types[i], src.sub_types[i]);
	}
	else
		dest->sub_types = NULL;
	return 1;
}

const int type_decl_sub_type(typecheck_type_t* super_type, typecheck_type_t sub_type) {
	ESCAPE_ON_NULL(super_type->sub_types);
	if (super_type->sub_type_count == TYPE_MAX_SUBTYPES)
		return 0;
	super_type->sub_types[super_type->sub_type_count++] = sub_type;
	return 1;
}

const int typecheck_type_compatible(typecheck_type_t target_type, typecheck_type_t match_type) {
	if (target_type.type < TYPE_SUPER_ARRAY)
		return target_type.type == match_type.type;
	else {
		if (target_type.type != match_type.type || target_type.sub_type_count != match_type.sub_type_count)
			return 0;
		for (uint_fast8_t i = 0; i < target_type.sub_type_count; i++)
			if (target_type.sub_types[i].type != match_type.sub_types[i].type)
				return 0;
		return 1;
	}
}