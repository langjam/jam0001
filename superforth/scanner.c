#include <ctype.h>
#include "hash.h"
#include "scanner.h"

#define SET_TOK_TYPE(TYPE) {scanner->last_tok.type = TYPE; break;}

static const char scanner_peek_char(scanner_t* scanner) {
	if (scanner->length == scanner->position)
		return 0;
	return scanner->source[scanner->position];
}

static const char scanner_read_char(scanner_t* scanner) {
	if (scanner->length == scanner->position)
		return scanner->last_char = 0;
	return scanner->last_char = scanner->source[scanner->position++];
}

void init_scanner(scanner_t* scanner, const char* source, const uint32_t length) {
	scanner->source = source;
	scanner->length = length;
	scanner->position = 0;
	scanner->last_err = ERROR_NONE;
	scanner_read_char(scanner);
	scanner_read_tok(scanner);
}

const int scanner_read_tok(scanner_t* scanner) {
	while (scanner->last_char == ' ' || scanner->last_char == '\t' || scanner->last_char == '\r' || scanner->last_char == '\n')
		scanner_read_char(scanner);

	scanner->last_tok.str = &scanner->source[scanner->position - 1];
	scanner->last_tok.length = 0;

	if (isalpha(scanner->last_char) || scanner->last_char == '_') {
		do {
			scanner_read_char(scanner);
			scanner->last_tok.length++;
		} while (isalpha(scanner->last_char) || isalnum(scanner->last_char) || scanner->last_char == '_');
		uint64_t id_hash = hash_s(scanner->last_tok.str, scanner->last_tok.length);
		switch (id_hash)
		{
		case 6385087377: //bool
			SET_TOK_TYPE(TOK_TYPECHECK_BOOL);
		case 6385115235: //char
			SET_TOK_TYPE(TOK_TYPECHECK_CHAR);
		case 193495088: //long
			SET_TOK_TYPE(TOK_TYPECHECK_LONG);
		case 210712519067: //float
			SET_TOK_TYPE(TOK_TYPECHECK_FLOAT);
		case 210706808356: //array
			SET_TOK_TYPE(TOK_TYPECHECK_ARRAY);
		case 6385593753: //proc
			SET_TOK_TYPE(TOK_TYPECHECK_PROC);
		case 6385058142: //auto
			SET_TOK_TYPE(TOK_AUTO);
		case 6953552265174: //global
			SET_TOK_TYPE(TOK_GLOBAL);
		case 5863476: //if
			SET_TOK_TYPE(TOK_IF);
		case 6385191717: //elif
			SET_TOK_TYPE(TOK_ELIF);
		case 6385192046: //else
			SET_TOK_TYPE(TOK_ELSE);
		case 210732529790: //while
			SET_TOK_TYPE(TOK_WHILE);
		case 6953974653989:
			SET_TOK_TYPE(TOK_RETURN);
		case 6385737701: //true
			SET_TOK_TYPE(TOK_TRUE);
		case 210712121072: //false
			SET_TOK_TYPE(TOK_FALSE);
		case 193486360: //and
			SET_TOK_TYPE(TOK_AND);
		case 5863686: //or
			SET_TOK_TYPE(TOK_OR);
		case 193500239:
			SET_TOK_TYPE(TOK_NEW);
		case 193504585: //rem
			do {
				scanner_read_char(scanner);
			} while (scanner->last_char != '\n');
			return scanner_read_tok(scanner);
		default:
			SET_TOK_TYPE(TOK_IDENTIFIER);
		}
	}
	else if (isalnum(scanner->last_char)) {
		do {
			scanner_read_char(scanner);
			scanner->last_tok.length++;
		} while (isalnum(scanner->last_char) || scanner->last_char == '.');
		scanner->last_tok.type = TOK_NUMERICAL;
	}
	else if (scanner->last_char == '\"') {
		scanner->last_tok.type = TOK_STRING;
		scanner->last_tok.str++;
		do {
			scanner_read_char(scanner);
			scanner->last_tok.length++;
			if (!scanner->last_char)
				PANIC(scanner, ERROR_UNEXPECTED_TOK);
		} while (scanner->last_char != '\"');
		scanner_read_char(scanner);
	}
	else if (scanner->last_char == '\'') {
		scanner->last_tok.type = TOK_CHAR;
		scanner->last_tok.str++;
		scanner->last_tok.length = 1;
		if(!scanner_read_char(scanner))
			PANIC(scanner, ERROR_UNEXPECTED_TOK);
		if (!scanner_read_char(scanner) || scanner->last_char != '\'')
			PANIC(scanner, ERROR_UNEXPECTED_TOK);
		scanner_read_char(scanner);
	}
	else {
		const char next_char = scanner_peek_char(scanner);
		
		switch (scanner->last_char)
		{
		case '+':
			SET_TOK_TYPE(TOK_ADD)
		case '-':
			SET_TOK_TYPE(TOK_SUBTRACT)
		case '*':
			SET_TOK_TYPE(TOK_MULTIPLY)
		case '/':
			SET_TOK_TYPE(TOK_DIVIDE)
		case '%':
			SET_TOK_TYPE(TOK_MODULO)
		case '^':
			SET_TOK_TYPE(TOK_POWER)
		case '=':
			if (next_char == '=') {
				scanner_read_char(scanner);
				SET_TOK_TYPE(TOK_EQUALS)
			}
			else
				SET_TOK_TYPE(TOK_SET)
		case '!':
			if (next_char == '=') {
				scanner_read_char(scanner);
				SET_TOK_TYPE(TOK_NOT_EQUAL)
			}
			else
				SET_TOK_TYPE(TOK_NOT)
		case '>':
			if (next_char == '=') {
				scanner_read_char(scanner);
				SET_TOK_TYPE(TOK_MORE_EQUAL);
			}
			else
				SET_TOK_TYPE(TOK_MORE)
		case '<':
			if (next_char == '=') {
				scanner_read_char(scanner);
				SET_TOK_TYPE(TOK_LESS_EQUAL)
			}
			else
				SET_TOK_TYPE(TOK_LESS)
		case '&':
			if (next_char == '&') {
				scanner_read_char(scanner);
				SET_TOK_TYPE(TOK_AND)
			}
			else
				PANIC(scanner, ERROR_UNEXPECTED_TOK)
		case '|':
			if (next_char == '|') {
				scanner_read_char(scanner);
				SET_TOK_TYPE(TOK_OR)
			}
			else
				PANIC(scanner, ERROR_UNEXPECTED_TOK)
		case '{':
			SET_TOK_TYPE(TOK_OPEN_BRACE);
		case '}':
			SET_TOK_TYPE(TOK_CLOSE_BRACE);
		case '(':
			SET_TOK_TYPE(TOK_OPEN_PAREN);
		case ')':
			SET_TOK_TYPE(TOK_CLOSE_PAREN);
		case '[':
			SET_TOK_TYPE(TOK_OPEN_BRACKET);
		case ']':
			SET_TOK_TYPE(TOK_CLOSE_BRACKET);
		case ',':
			SET_TOK_TYPE(TOK_COMMA);
		case 0:
			SET_TOK_TYPE(TOK_EOF);
		default:
			PANIC(scanner, ERROR_UNEXPECTED_TOK);
		}
		scanner_read_char(scanner);
	}
	return 1;
}