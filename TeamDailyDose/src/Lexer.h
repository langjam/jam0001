#pragma once

#include "AK/Format.h"
#include <AK/ByteBuffer.h>
#include <AK/GenericLexer.h>
#include <AK/Result.h>
#include <AK/String.h>
#include <AK/StringView.h>
#include <AK/Variant.h>

#define ENUMERATE_TOKENS                            \
    __ENUMERATE_TOKEN(ParenOpen, "("sv)             \
    __ENUMERATE_TOKEN(ParenClose, ")"sv)            \
    __ENUMERATE_TOKEN(BracketOpen, "["sv)           \
    __ENUMERATE_TOKEN(BracketClose, "]"sv)          \
    __ENUMERATE_TOKEN(Colon, ":"sv)                 \
    __ENUMERATE_TOKEN(Period, "."sv)                \
    __ENUMERATE_TOKEN(Pipe, "|"sv)                  \
    __ENUMERATE_TOKEN(Identifier, "<identifier>"sv) \
    __ENUMERATE_TOKEN(Comment, "<comment>"sv)       \
    __ENUMERATE_TOKEN(Eof, "<end of file>"sv)

enum class TokenType {
#define __ENUMERATE_TOKEN(i, s) i,
    ENUMERATE_TOKENS
#undef __ENUMERATE_TOKEN
};

inline StringView token_to_string(TokenType token)
{
    switch (token) {
#define __ENUMERATE_TOKEN(i, s) \
    case TokenType::i:          \
        return s;
        ENUMERATE_TOKENS
#undef __ENUMERATE_TOKEN
    default:
        VERIFY_NOT_REACHED();
    }
}

enum IdentifierTag {
    TIdentifier,
};

enum CommentTag {
    TComment,
};

class Token {
public:
    Token(IdentifierTag, String value)
        : m_type(TokenType::Identifier)
        , m_string(value)
    {
    }
    Token(CommentTag, String value)
        : m_type(TokenType::Comment)
        , m_string(value)
    {
    }
    Token(TokenType type)
        : m_type(type)
    {
        VERIFY(type != TokenType::Identifier && type != TokenType::Comment);
    }

    TokenType type() const { return m_type; }
    String const& string() const { return m_string; }

private:
    TokenType m_type;
    String m_string;
};

class Location {
public:
    Location operator=(Location const& other)
    {
        m_line = other.m_line;
        m_column = other.m_column;
        return *this;
    }

    int line() const { return m_line; }
    int column() const { return m_column; }

    void advance_line()
    {
        m_line++;
        m_column = 1;
    }
    void advance_column() { m_column++; }

    void advance_by_string(StringView const& text);

private:
    int m_line { 1 };
    int m_column { 1 };
};

class Lexer {
public:
    Lexer(StringView const& input)
        : m_lexer(input)
    {
    }

    Result<TokenType, String> peek_token() const;
    Result<Token, String> next_token();
    void consume_token() { VERIFY(!next_token().is_error()); }
    Location const& location() const { return m_location; }
    bool is_eof() const;
    void backtrack(Token const& token);

private:
    GenericLexer m_lexer;
    Location m_location;
    Location m_next_location;
};

namespace AK {

template<>
struct Formatter<Location> : Formatter<FormatString> {
    void format(FormatBuilder& builder, Location const& location)
    {
        return Formatter<FormatString>::format(builder, "{}:{}", location.line(), location.column());
    }
};

}
