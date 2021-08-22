#include "Lexer.h"

#include <sstream>
#include <cassert>

#include "util.h"

namespace fun {

static char parseDigit(char ch, int radix) {
	int num;
	if (ch >= '0' && ch <= '9') {
		num = ch - '0';
	} else if (ch >= 'a' && ch <= 'z') {
		num = ch - 'a' + 10;
	} else if (ch >= 'A' && ch <= 'Z') {
		num = ch - 'A' + 10;
	} else {
		return -1;
	}

	if (num < radix) {
		return num;
	}

	return -1;
}

static double parseDigitString(const std::string &digits, int radix) {
	double num = 0;
	double mult = 1;
	for (ssize_t idx = digits.size() - 1; idx >= 0; --idx) {
		num += digits[idx] * mult;
		mult *= radix;
	}

	return num;
}

std::string Token::toString() {
	std::string str;
	str += std::to_string(line);
	str += ":";
	str += std::to_string(column);
	str += ": ";

	str += kindToString(kind);

	if (std::holds_alternative<std::string>(val)) {
		str += "; string: '";
		str += std::get<std::string>(val);
		str += '\'';
	} else if (std::holds_alternative<double>(val)) {
		str += "; number: ";
		str += std::to_string(std::get<double>(val));
	}

	return str;
}

std::string Token::kindToString(TokKind kind) {
	switch (kind) {
	case TokKind::IDENT: return "IDENT";
	case TokKind::NUMBER: return "NUMBER";
	case TokKind::STRING: return "STRING";
	case TokKind::OPEN_BRACE: return "OPEN_BRACE";
	case TokKind::CLOSE_BRACE: return "CLOSE_BRACE";
	case TokKind::OPEN_PAREN: return "OPEN_PAREN";
	case TokKind::CLOSE_PAREN: return "CLOSE_PAREN";
	case TokKind::OPEN_BRACKET: return "OPEN_BRACKET";
	case TokKind::CLOSE_BRACKET: return "CLOSE_BRACKET";
	case TokKind::BACKSLASH: return "BACKSLASH";
	case TokKind::SEMICOLON: return "SEMICOLON";
	case TokKind::COMMA: return "COMMA";
	case TokKind::DOT: return "DOT";
	case TokKind::EQEQ: return "EQEQ";
	case TokKind::NOTEQ: return "NOTEQ";
	case TokKind::GT: return "GT";
	case TokKind::GTEQ: return "GTEQ";
	case TokKind::LT: return "LT";
	case TokKind::LTEQ: return "LTEQ";
	case TokKind::COLONEQ: return "COLONEQ";
	case TokKind::EQ: return "EQ";
	case TokKind::PLUS: return "PLUS";
	case TokKind::PLUSEQ: return "PLUSEQ";
	case TokKind::MINUS: return "MINUS";
	case TokKind::MINUSEQ: return "MINUSEQ";
	case TokKind::MULT: return "MULT";
	case TokKind::MULTEQ: return "MULTEQ";
	case TokKind::DIV: return "DIV";
	case TokKind::DIVEQ: return "DIVEQ";
	case TokKind::COLONCOLON: return "COLONCOLON";
	case TokKind::IF: return "IF";
	case TokKind::ELSE: return "ELSE";
	case TokKind::WHILE: return "WHILE";
	case TokKind::RETURN: return "RETURN";
	case TokKind::E_O_F: return "E_O_F";
	}

	return "(unknown)";
}

Token &Lexer::peek(size_t n) {
	assert(n < sizeof(buffer_) / sizeof(*buffer_));
	while (n >= bufidx_) {
		buffer_[bufidx_++] = readTok();
	}

	return buffer_[n];
}

Token Lexer::consume() {
	if (bufidx_ == 0) {
		buffer_[bufidx_++] = readTok();
	}

	Token tok = std::move(buffer_[0]);
	for (size_t i = 0; i < bufidx_ - 1; ++i) {
		buffer_[i] = std::move(buffer_[i + 1]);
	}
	bufidx_ -= 1;

	return tok;
}

void Lexer::reset() {
	reader_.reset();
	bufidx_ = 0;
}

void Lexer::skipWhitespace() {
	while (true) {
		int ch = peekCh(0);
		if (ch != ' ' && ch != '\t' && ch != '\n') {
			break;
		}

		readCh();
	}
}

Token Lexer::readString() {
	std::string str;
	char terminator = readCh();

	while (true) {
		int ch = readCh();
		if (ch == EOF) {
			error("Unexpected EOF");
		}

		if (ch == '\\') {
			ch = readCh();
			if (ch == EOF) {
				error("Unexpected EOF");
			}

			if (ch == 'n') {
				str += '\n';
			} else if (ch == 't') {
				str += '\t';
			} else if (ch == 'r') {
				str += '\r';
			} else if (ch == 'e') {
				str += 0x1b; // Escape character
			} else if (ch == '\'' || ch == '"' || ch == '\\') {
				str += ch;
			} else {
				error(concat("Unexpected escape character '", (char)ch, '\''));
			}
		} else if (ch == terminator) {
			return makeTok(TokKind::STRING, std::move(str));
		} else {
			str += ch;
		}
	}
}

Token Lexer::readNumber() {
	int radix = 10;
	if (peekCh(0) == 0 && peekCh(1) == 'x') {
		radix = 16;
		readCh(); readCh();
	} else if (peekCh(0) == 0 && peekCh(1) == 'b') {
		radix = 2;
		readCh(); readCh();
	} else if (peekCh(0) == 0 && peekCh(1) == 'o') {
		radix = 8;
		readCh(); readCh();
	}

	if (parseDigit(peekCh(0), radix) < 0) {
		error("Invalid number");
	}

	char digit;
	std::string digits = "";

	double integral;
	while ((digit = parseDigit(peekCh(0), radix)) >= 0) {
		digits += digit;
		readCh();
	}
	integral = parseDigitString(digits, radix);

	double fractional = 0;
	if (peekCh(0) == '.' && parseDigit(peekCh(1), radix) >= 0) {
		readCh();
		digits = "";
		while ((digit = parseDigit(peekCh(0), radix)) >= 0) {
			digits += digit;
			readCh();
		}
		fractional = parseDigitString(digits, radix);

		double div = 1;
		for (size_t i = 0; i < digits.size(); ++i) {
			div *= radix;
		}

		fractional /= div;
	}

	return makeTok(TokKind::NUMBER, integral + fractional);
}

Token Lexer::readIdent() {
	std::string str;
	str += readCh();

	while (true) {
		int ch = peekCh(0);
		if (!(
				(ch >= 'a' && ch <= 'z') ||
				(ch >= 'A' && ch <= 'Z') ||
				(ch >= '0' && ch <= '9') ||
				ch == '_')) {
			break;
		}

		str += ch;
		readCh();
	}

	return makeTok(TokKind::IDENT, std::move(str));
}

Token Lexer::readTok() {
	skipWhitespace();

	tokStartIdx_ = reader_.idx;

	int ch = peekCh(0);
	switch (ch) {
	case '{': readCh(); return makeTok(TokKind::OPEN_BRACE);
	case '}': readCh(); return makeTok(TokKind::CLOSE_BRACE);
	case '(': readCh(); return makeTok(TokKind::OPEN_PAREN);
	case ')': readCh(); return makeTok(TokKind::CLOSE_PAREN);
	case '[': readCh(); return makeTok(TokKind::OPEN_BRACKET);
	case ']': readCh(); return makeTok(TokKind::CLOSE_BRACKET);
	case '\\': readCh(); return makeTok(TokKind::BACKSLASH);
	case ';': readCh(); return makeTok(TokKind::SEMICOLON);
	case ',': readCh(); return makeTok(TokKind::COMMA);
	case '.': readCh(); return makeTok(TokKind::DOT);
	case EOF: readCh(); return makeTok(TokKind::E_O_F);
	}

	int ch2 = peekCh(1);
	if (ch == '=' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::EQEQ);
	} else if (ch == '!' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::NOTEQ);
	} else if (ch == '>' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::GTEQ);
	} else if (ch == '>') {
		readCh();
		return makeTok(TokKind::GT);
	} else if (ch == '<' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::LTEQ);
	} else if (ch == '<') {
		readCh();
		return makeTok(TokKind::LT);
	} else if (ch == ':' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::COLONEQ);
	} else if (ch == '=') {
		readCh();
		return makeTok(TokKind::EQ);
	} else if (ch == '+' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::PLUSEQ);
	} else if (ch == '+') {
		readCh();
		return makeTok(TokKind::PLUS);
	} else if (ch == '-' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::MINUSEQ);
	} else if (ch == '-') {
		readCh();
		return makeTok(TokKind::MINUS);
	} else if (ch == '*' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::MULTEQ);
	} else if (ch == '*') {
		readCh(); readCh();
		return makeTok(TokKind::MULT);
	} else if (ch == '/' && ch2 == '=') {
		readCh(); readCh();
		return makeTok(TokKind::DIVEQ);
	} else if (ch == '/') {
		readCh();
		return makeTok(TokKind::DIV);
	} else if (ch == ':' && ch2 == ':') {
		readCh(); readCh();
		return makeTok(TokKind::COLONCOLON);
	}

	if (ch == '"' || ch == '\'') {
		return readString();
	} else if (ch >= '0' && ch <= '9') {
		return readNumber();
	} else if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_') {
		Token ident = readIdent();
		if (ident.kind != TokKind::IDENT) {
			return ident;
		}

		std::string str = std::get<std::string>(ident.val);
		if (str == "if") {
			return makeTok(TokKind::IF);
		} else if (str == "else") {
			return makeTok(TokKind::ELSE);
		} else if (str == "while") {
			return makeTok(TokKind::WHILE);
		} else if (str == "return") {
			return makeTok(TokKind::RETURN);
		}

		return ident;
	}

	error(concat("Unexpected character: '", (char)ch, '\''));
}

int Lexer::readCh() {
	return reader_.readCh();
}

int Lexer::peekCh(size_t n) {
	return reader_.peekCh(n);
}

}
