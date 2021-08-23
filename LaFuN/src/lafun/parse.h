#pragma once

#include <string>

#include "fun/Lexer.h"
#include "fun/IdentResolver.h"
#include "ast.h"

namespace lafun {

struct LafunParseError: public std::exception {
	LafunParseError(int line, int column, std::string message):
		line(line), column(column), message(message) {}
	int line;
	int column;
	std::string message;

	const char *what() const noexcept override { return message.c_str(); }
};

void parseLafun(Reader &reader, ast::LafunDocument &document);

}
