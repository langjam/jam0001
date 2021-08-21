#ifndef ECHOES_LEXER_H
#define ECHOES_LEXER_H

#include <stddef.h>
#include <stdbool.h>

enum TokenName {
    TokenNameString = 1,
    TokenNameNumber,
    TokenNameNewline,
    TokenNameLog,
    TokenNameKey,
    TokenNameAdd,
    TokenNameSub,
    TokenNameMul,
    TokenNameDiv,
    TokenNameLeftParen,
    TokenNameRightParen,
    TokenNameRoutine
};

struct Token {
    enum TokenName name;
    char *string;
    size_t length;
};

struct Lexer {
    struct Token token;
    char *stream;
    size_t line;
    size_t column;
};

bool lexer_tokenize(struct Lexer* const lexer);

#endif // ECHOES_LEXER_H
