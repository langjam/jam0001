#include "lexer.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

static inline bool lexer_error(const struct Lexer* const lexer, const char* const error_message) {
    printf("LexerError: %s. on line: %ld, column: %ld\n", error_message, lexer->line, lexer->column);
    exit(1);
}

static inline bool is_identifier_char(const char c) {
    return isalnum(c) || c == '_';
}

static bool lexer_clean(struct Lexer* const lexer) {
    bool cleaned = false;
    while (lexer->stream[0] == ' ' || lexer->stream[0] == '\t') {
        cleaned = true;
        ++lexer->stream;
        ++lexer->column;
    }
    return cleaned;
}

static bool lexer_match_keyword(struct Lexer* const lexer, const char* const keyword,
                                        const size_t length, const enum TokenName name) {
    if (strncmp(lexer->stream, keyword, length) == 0) {
        lexer->token.name = name;
        lexer->token.string = lexer->stream;
        lexer->token.length = length;
        lexer->stream += length;
        lexer->column += length;
        return true;
    }
    return false;
}

bool lexer_tokenize(struct Lexer* const lexer) {
    lexer_clean(lexer);
    // if it's an eof, return false
    if (lexer->stream[0] == '\0')
        return false;
    // tokenize a bunch of newlines as one newline
    if (lexer->stream[0] == '\n') {
        lexer->token.name = TokenNameNewline;
        lexer->token.string = lexer->stream;
        lexer->token.length = 1;
        do {
            ++lexer->stream;
            ++lexer->line;
            lexer->column = 1;
            lexer_clean(lexer);
        } while(lexer->stream[0] == '\n');
        // if had newlines and was ended with a \0, count it as an eof
        if (lexer->stream[0] == '\0')
            return false;
        return true;
    }
    if (lexer->stream[0] == ':') {
        ++lexer->stream;
        ++lexer->column;
        lexer->token.string = lexer->stream;
        lexer->token.length = 0;
        lexer->token.name = TokenNameKey;
        if (!is_identifier_char(lexer->stream[0])) {
            lexer_error(lexer, "Unexpected character after ':'");
        }
        do {
            ++lexer->stream;
            ++lexer->column;
            ++lexer->token.length;
        } while(is_identifier_char(lexer->stream[0]));
        return true;
    }
    // tokenize number
    if (isdigit(lexer->stream[0])) {
        lexer->token.name = TokenNameNumber;
        lexer->token.string = lexer->stream;
        lexer->token.length = 0;
        do {
            ++lexer->token.length;
            ++lexer->stream;
            ++lexer->column;
        } while(isdigit(lexer->stream[0]));
        if (lexer->stream[0] == '.') {
            do {
                ++lexer->token.length;
                ++lexer->stream;
                ++lexer->column;
            } while (isdigit(lexer->stream[0]));
        }
        return true;
    }
    // tokenize string
    if (lexer->stream[0] == '"') {
        lexer->token.name = TokenNameString;
        lexer->token.string = ++lexer->stream;
        ++lexer->column;
        lexer->token.length = 0;
        while (lexer->stream[0] != '"') {
            if (lexer->stream[0] == '\n' || lexer->stream[0] == '\0')
                lexer_error(lexer, "Unexpected end-of-line");
            ++lexer->token.length;
            ++lexer->stream;
            ++lexer->column;
        }
        ++lexer->stream;
        ++lexer->column;
        return true;
    }
    if (lexer->stream[0] == '+') {
        lexer->token.name = TokenNameAdd;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '-') {
        lexer->token.name = TokenNameSub;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '*') {
        lexer->token.name = TokenNameMul;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '/') {
        lexer->token.name = TokenNameDiv;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '(') {
        lexer->token.name = TokenNameLeftParen;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == ')') {
        lexer->token.name = TokenNameRightParen;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == ',') {
        lexer->token.name = TokenNameComma;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '{') {
        lexer->token.name = TokenNameLeftCur;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '}') {
        lexer->token.name = TokenNameRightCur;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '=') {
        lexer->token.name = TokenNameEquals;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '<') {
        lexer->token.name = TokenNameSmallerThan;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    if (lexer->stream[0] == '>') {
        lexer->token.name = TokenNameBiggerThan;
        lexer->token.length = 1;
        lexer->token.string = lexer->stream++;
        return true;
    }
    // try to tokenize `log`
    if (lexer_match_keyword(lexer, "log", 3, TokenNameLog))
        return true;
    // try to tokenize `routine`
    if (lexer_match_keyword(lexer, "routine", 7, TokenNameRoutine))
        return true;
    // try to tokenize `if`
    if (lexer_match_keyword(lexer, "if", 2, TokenNameIf))
        return true;
    // try to tokenize `else`
    if (lexer_match_keyword(lexer, "else", 4, TokenNameElse))
        return true;
    // try to tokenize `loop`
    if (lexer_match_keyword(lexer, "loop", 4, TokenNameLoop))
        return true;
    lexer_error(lexer, "Unrecognized token");
    return false;
}

char *token_allocate_key(struct Token* const token) {
    char *key;
    assert(token->name == TokenNameKey);
    key = malloc((token->length + 1) * sizeof(char));
    strncpy(key, token->string, token->length);
    key[token->length] = '\0';
    return key;
}
