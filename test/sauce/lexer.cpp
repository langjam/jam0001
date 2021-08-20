#include "lexer.h"
#include <AK/CharacterTypes.h>
#include <AK/FileStream.h>

Result<Token, LexError> Lexer::next()
{
    InputFileStream stream { stdin };

    m_current_token = {};
    m_current_token.source_range.start = m_current_source_position;
    char ch;
    for (;;) {
        if (m_next_input.has_value()) {
            ch = m_next_input.release_value();
        } else if (!stream.read_or_error({ &ch, 1 })) {
            stream.handle_any_error();
            return emit_token(Token::Type::Eof, ""sv);
        }

        if (ch == '\n') {
            m_current_source_position.column = 0;
            m_current_source_position.line++;
        } else {
            m_current_source_position.column++;
        }

        switch (m_state) {
        case Free:
            if (ch == '/')
                m_state = CouldBeInComment;
            else if (is_ascii_space(ch))
                continue;
            else if (ch == '(')
                return emit_token(Token::Type::OpenParen, "("sv);
            else if (ch == ')')
                return emit_token(Token::Type::CloseParen, ")"sv);
            else if (ch == '{')
                return emit_token(Token::Type::OpenBrace, "{"sv);
            else if (ch == '}')
                return emit_token(Token::Type::OpenBrace, "}"sv);
            else if (ch == ':')
                return emit_token(Token::Type::Colon, ":"sv);
            else if (ch == ';')
                return emit_token(Token::Type::Semicolon, ";"sv);
            else if (ch == '<')
                return emit_token(Token::Type::MentionOpen, "<"sv);
            else if (ch == '>')
                return emit_token(Token::Type::MentionClose, ">"sv);
            else if (ch == ',')
                return emit_token(Token::Type::Comma, ","sv);
            else if (ch == '.')
                return emit_token(Token::Type::Dot, "."sv);
            else if (ch == '|')
                return emit_token(Token::Type::Pipe, "|"sv);
            else if (ch == '=')
                return emit_token(Token::Type::Equals, "="sv);
            else if (is_ascii_digit(ch)) {
                m_state = InInteger;
                m_string_builder.append(ch);
                m_current_token.type = Token::Type::Integer;
            } else if (ch == '!')
                m_state = CouldBeInIndirectCommentMention;
            else if (ch == '"') {
                m_state = InString;
                m_current_token.type = Token::Type::String;
                m_current_token.source_range.start = m_current_source_position;
            } else {
                m_state = InIdentifier;
                m_string_builder.append(ch);
                m_current_token.type = Token::Type::Identifier;
                m_current_token.source_range.start = m_current_source_position;
            }
            break;
        case InIdentifier:
            if (!R"(:(){} <>!"=;,.|)"sv.contains(ch)) {
                m_string_builder.append(ch);
                continue;
            }

            m_next_input = ch;
            m_state = Free;
            m_current_token.source_range.end = m_current_source_position;
            m_current_token.text = m_string_builder.build();
            m_string_builder.clear();
            return m_current_token;
        case CouldBeInComment:
            if (ch == '/') {
                m_state = InComment;
                m_current_token.type = Token::Type::Comment;
            } else {
                m_next_input = ch;
                m_current_token.type = Token::Type::Identifier;
                m_current_token.text = "/"sv;
                m_state = InIdentifier;
            }
            break;
        case InComment:
            if (ch == '\n') {
                m_current_token.text = m_string_builder.build();
                m_string_builder.clear();
                m_state = Free;
                m_current_token.source_range.end = m_current_source_position;
                return m_current_token;
            }

            m_string_builder.append(ch);
            break;
        case InString:
            if (ch == '"') {
                m_current_token.text = m_string_builder.build();
                m_string_builder.clear();
                m_state = Free;
                m_current_token.source_range.end = m_current_source_position;
                return m_current_token;
            }
            m_string_builder.append(ch);
            break;
        case InInteger:
            if (!is_ascii_digit(ch)) {
                m_current_token.text = m_string_builder.build();
                m_string_builder.clear();
                m_state = Free;
                m_current_token.source_range.end = m_current_source_position;
                m_next_input = ch;
                return m_current_token;
            }
            m_string_builder.append(ch);
            break;
        case CouldBeInIndirectCommentMention:
            if (ch == '<') {
                m_state = Free;
                return emit_token(Token::Type::IndirectMentionOpen, "!<"sv);
            }
            m_next_input = ch;
            m_current_token.type = Token::Type::Identifier;
            m_current_token.text = "/";
            m_state = InIdentifier;
            break;
        }
    }
}

LexError Lexer::make_error_here(String text)
{
    return {
        move(text),
        m_current_source_position
    };
}

Token Lexer::emit_token(Token::Type type, String text)
{
    return {
        move(text),
        type,
        {
            m_current_token.source_range.start,
            m_current_source_position,
        },
    };
}
