#include "Lexer.h"
#include <AK/CharacterTypes.h>
#include <AK/Format.h>
#include <AK/ScopeGuard.h>
#include <AK/StringBuilder.h>

void Location::advance_by_string(StringView const& text)
{
    for (auto ch : text) {
        if (ch == '\n') {
            advance_line();
        } else {
            advance_column();
        }
    }
}

Result<TokenType, String> Lexer::peek_token() const
{
    if (m_lexer.is_eof())
        return TokenType::Eof;

    int index = 0;
    int peek;
    do {
        peek = m_lexer.peek(index++);
    } while (is_ascii_space(peek));

    if (peek == '\0') {
        return TokenType::Eof;
    }

    if (is_ascii_alpha(peek) || peek == '_') {
        return TokenType::Identifier;
    }

    switch (peek) {
    case '"':
        return TokenType::Comment;
    case '(':
        return TokenType::ParenOpen;
    case ')':
        return TokenType::ParenClose;
    case '[':
        return TokenType::BracketOpen;
    case ']':
        return TokenType::BracketClose;
    case ':':
        return TokenType::Colon;
    case '.':
        return TokenType::Period;
    case '|':
        return TokenType::Pipe;
    default:
        return String::formatted("Unknown character {}", peek);
    }
}

Result<Token, String> Lexer::next_token()
{
    m_location = m_next_location;

    // Skip any whitespace.
    m_next_location.advance_by_string(m_lexer.consume_until([](char c) { return !is_ascii_space(c); }));

    if (m_lexer.is_eof()) {
        return { TokenType::Eof };
    }

    auto peek = m_lexer.peek();

    // Parse any comments (obviously :^).
    if (peek == '"') {
        m_next_location.advance_column();
        StringView comment = m_lexer.consume_quoted_string('\\');
        if (comment.is_null()) {
            return String { "Unterminated comment literal" };
        }

        // Parse escapes.
        StringBuilder builder;
        size_t offset = 0;
        while (true) {
            auto maybe_offset = comment.find('\\', offset);
            if (!maybe_offset.has_value()) {
                auto final_part = comment.substring_view(offset);
                m_next_location.advance_by_string(final_part);
                builder.append(final_part);
                break;
            }

            auto escape_offset = maybe_offset.value();
            auto part_before_escape = comment.substring_view(offset, escape_offset - offset);
            m_next_location.advance_by_string(part_before_escape);
            builder.append(part_before_escape);

            // NOTE: This is safe, because the escape character at the end of
            //       the string would've escaped the closing quote.
            switch (comment[escape_offset + 1]) {
            case '\\':
                builder.append('\\');
                break;
            case '"':
                builder.append('"');
                break;
            default:
                return String::formatted("Unknown escape character \\{}", comment[escape_offset + 1]);
            }

            offset = escape_offset + 2;
        }

        // For the closing quote.
        m_next_location.advance_column();

        return Token { TComment, builder.to_string() };
    }

    // Parse an identifier.
    if (is_ascii_alpha(peek) || peek == '_') {
        StringBuilder builder;
        builder.append(peek);
        m_next_location.advance_column();
        m_lexer.ignore();

        StringView rest_of_identifier = m_lexer.consume_until([](char c) { return !is_ascii_alpha(c) && !AK::is_ascii_digit(c); });
        m_next_location.advance_by_string(rest_of_identifier);
        builder.append(rest_of_identifier);

        return Token { TIdentifier, builder.to_string() };
    }

    m_lexer.ignore();
    m_next_location.advance_column();

    // Parse the rest of the tokens.
    switch (peek) {
    case '(':
        return { TokenType::ParenOpen };
    case ')':
        return { TokenType::ParenClose };
    case '[':
        return { TokenType::BracketOpen };
    case ']':
        return { TokenType::BracketClose };
    case ':':
        return { TokenType::Colon };
    case '.':
        return { TokenType::Period };
    case '|':
        return { TokenType::Pipe };
    default:
        return String::formatted("Unknown character {}", peek);
    }
}

bool Lexer::is_eof() const
{
    if (m_lexer.is_eof())
        return true;

    auto maybe_peek = peek_token();
    if (maybe_peek.is_error()) {
        // NOTE: Error condition means we have more tokens after, so EOF is
        //       false, technically.
        return false;
    }

    return maybe_peek.value() == TokenType::Eof;
}

void Lexer::backtrack(Token const& token)
{
    switch (token.type()) {
    case TokenType::ParenOpen:
    case TokenType::ParenClose:
    case TokenType::Colon:
    case TokenType::Period:
    case TokenType::Pipe:
        m_lexer.retreat();
        break;
    case TokenType::Comment:
        m_lexer.retreat(token.string().length() + 2);
        break;
    case TokenType::Identifier:
        m_lexer.retreat(token.string().length());
        break;
    default:
        VERIFY_NOT_REACHED();
    }
}
