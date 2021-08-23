#pragma once

#include <stdexcept>
#include <string>

#include "ast.h"
#include "Lexer.h"

namespace fun {

struct ParseError: public std::exception {
	ParseError(int line, int column, std::string message):
			line(line), column(column) {
		error = std::to_string(line);
		error += ":";
		error += std::to_string(column);
		error += ": ";
		error += message;
	}

	int line;
	int column;
	std::string error;

	const char *what() const noexcept override {
		return error.c_str();
	}
};

void parseCodeBlock(Lexer &lexer, ast::CodeBlock &block);
void parseDeclaration(Lexer &lexer, ast::Declaration &decl);

}
