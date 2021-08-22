#pragma once

#include <string>
#include <cstddef>
#include <variant>
#include <vector>

#include "Reader.h"
#include "ByteRange.h"

namespace fun {

struct LexError: public std::exception {
	LexError(int line, int column, std::string message):
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

enum class TokKind {
	IDENT,
	NUMBER,
	STRING,

	OPEN_BRACE,
	CLOSE_BRACE,
	OPEN_PAREN,
	CLOSE_PAREN,
	OPEN_BRACKET,
	CLOSE_BRACKET,

	BACKSLASH,
	SEMICOLON,
	COMMA, DOT,
	EQEQ, NOTEQ,
	GT, GTEQ,
	LT, LTEQ,
	COLONEQ, EQ,
	PLUS, PLUSEQ,
	MINUS, MINUSEQ,
	MULT, MULTEQ,
	DIV, DIVEQ,
	COLONCOLON,

	IF, ELSE, WHILE, RETURN,

	E_O_F,
};

struct Token {
	TokKind kind;
	int line;
	int column;
	ByteRange range;

	struct Empty {};
	std::variant<Empty, std::string, double> val;

	std::string &getStr() { return std::get<std::string>(val); }
	double getNum() { return std::get<double>(val); }

	std::string toString();
	static std::string kindToString(TokKind kind);
};

class Lexer {
public:
	Lexer(std::string_view string): reader_(string) {}
	Lexer(const Reader &reader): reader_(reader) {}

	Token &peek(size_t n);
	Token consume();

	void reset();

	Reader reader_;

private:
	void skipWhitespace();

	Token readString();
	Token readNumber();
	Token readIdent();

	Token readTok();

	Token makeTok(TokKind kind) {
		return {kind, reader_.line, reader_.column, {tokStartIdx_, reader_.idx}, {}};
	}

	Token makeTok(TokKind kind, std::string &&str) {
		return {kind, reader_.line, reader_.column, {tokStartIdx_, reader_.idx}, std::move(str)};
	}

	Token makeTok(TokKind kind, double num) {
		return {kind, reader_.line, reader_.column, {tokStartIdx_, reader_.idx}, num};
	}

	[[noreturn]]
	void error(std::string &&message) { throw LexError(reader_.line, reader_.column, std::move(message)); }

	int readCh();
	int peekCh(size_t n);

	Token buffer_[4];
	size_t bufidx_ = 0;
	size_t tokStartIdx_;
};

}
