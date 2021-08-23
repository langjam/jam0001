#pragma once

#include <cstddef>

struct CodePos {
	size_t line;
	size_t col;

	CodePos() = default;
	CodePos(const size_t line, const size_t col) : line{line}, col{col} {}
};
