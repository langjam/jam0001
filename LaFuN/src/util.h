#pragma once

#include <string>
#include <sstream>

// Visitor helper type
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
// Deduction guide
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// Concatenate strings
template<typename... Args>
std::string concat(Args &&...args) {
	std::stringstream stream;
	((stream << args), ...);
	return stream.str();
}
