#pragma once

#include "CodePos.h"

#include <string>

#define TRY(x) do { const Error _error = (x); if (_error) return _error; } while (false)

struct Error {
	bool error;
	std::string message;
	CodePos pos;

	operator bool() const { return error; }

private:
	Error(const bool error, std::string message, const CodePos pos) : error{error}, message{std::move(message)}, pos{pos} {}
public:
	Error(std::string message, const CodePos pos) : error{true}, message{std::move(message)}, pos{pos} {}
	static const Error None;
};

inline const Error Error::None = Error{false, std::string{}, CodePos{}};
