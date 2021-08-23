#pragma once

#include <variant>
#include <vector>

#include "fun/ast.h"

namespace lafun::ast {

struct FunBlock {
	fun::ast::Declaration decl;
	fun::ByteRange range;
};

struct RawLatex {
	std::string str;
};

struct IdentifierUpwardsRef {
	std::string ident;
	size_t id = 0;
};

struct IdentifierDownwardsRef {
	std::string ident;
	size_t id = 0;
};

using LafunBlock = std::variant<
	FunBlock,
	RawLatex,
	IdentifierUpwardsRef,
	IdentifierDownwardsRef>;

struct LafunDocument {
	std::vector<LafunBlock> blocks;
	std::vector<const fun::ast::Identifier *> defs;
	std::vector<const fun::ast::Identifier *> refs;
};

}
