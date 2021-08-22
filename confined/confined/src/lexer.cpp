#include "lexer.h"
#include "parser.h"

#include <iostream>

#define BASE2_DIGITS S("0123456789")
#define BASE10_DIGITS S("0123456789")
#define BASE16_DIGITS S("0123456789abcdef")

#define RESERVED_CHARS S("~`!@#$%^&*()+-=[]\\|;:\"',<>./? \t\n")
#define STRING_LITERIAL_DECLARATION_CHARS S("\"'")

static Token lexer_parse_number(Lexer& lexer)
{
    Token token;
    str_t digits;
    str_t number;

    if(lexer.current() == S("0b"))
    {
        token.set_type(Token::TOKEN_BINARY);
        digits = BASE2_DIGITS;

        lexer.advance(2);
    }
    else if(lexer.current() == S("0x"))
    {
        token.set_type(Token::TOKEN_HEXADECIMAL);
        digits = BASE16_DIGITS;

        lexer.advance(2);
    }
    else
    {
        token.set_type(Token::TOKEN_DECIMAL);
        digits = BASE10_DIGITS;
    }

    while(lexer.current().is(digits) && lexer)
    {
        number.append(1, lexer.current());

        if(!lexer.peek().is(digits)) break;

        lexer.advance();
    }

    token.set_data(number);

    return token;
}

static Token lexer_parse_identifier(Lexer& lexer)
{
    Token token = Token::TOKEN_IDENTIFIER;
    str_t identifier;

    while(lexer.current().is_not(RESERVED_CHARS) && lexer)
    {
        identifier.append(1, lexer.current());

        if(lexer.peek().is(RESERVED_CHARS)) break;

        lexer.advance();
    }

    token.set_data(identifier);

    return token;
}

static Token lexer_parse_string_literial(Lexer& lexer)
{
    Token token = Token::TOKEN_STRING_LITERIAL;
    str_t string;

    lexer.advance();

    while(lexer.current().is_not(STRING_LITERIAL_DECLARATION_CHARS) && lexer)
    {
        string.append(1, lexer.current());

        lexer.advance();
    }

    token.set_data(string);

    return token;
}

LexerInfo lang_lex(const str_t& str)
{
    LexerInfo lexer_info = {};
    Lexer lexer(str);

    auto add_token = [&](const Token& token)
    {
        lexer_info.token_info.push_back({ .token = token, .line_number = lexer.get_line_number(), .column_number = lexer.get_column_number() });
    };

    for(char_t c : lexer)
    {
        // cycle through three times to make sure all tokens are reached
        for(int i = 0; i < 3; i++)
        {
            lexer.skip(' ', [&]()
            {
                add_token(Token::TOKEN_WHITESPACE);
            })
            .skip('\t', [&]()
            {
                add_token(Token::TOKEN_TAB);
            })
            .skip('\n', [&]()
            {
                add_token(Token::TOKEN_NEW_LINE);
            })
            .skip(S("++"), [&]()
            {
                add_token(Token::TOKEN_INCREMENT);
            })
            .skip(S("--"), [&]()
            {
                add_token(Token::TOKEN_DECREMENT);
            })
            .skip('+', [&]()
            {
                add_token(Token::TOKEN_ADD);
            })
            .skip('-', [&]()
            {
                add_token(Token::TOKEN_SUBTRACT);
            })
            .skip('*', [&]()
            {
                add_token(Token::TOKEN_MULTIPLY);
            })
            .skip('/', [&]()
            {
                add_token(Token::TOKEN_DIVIDE);
            })
            .skip('=', [&]()
            {
                add_token(Token::TOKEN_EQUALS);
            })
            .skip('!', [&]()
            {
                add_token(Token::TOKEN_EXCLAIM_MARK);
            })
            .skip('{', [&]()
            {
                add_token(Token::TOKEN_L_CURLY_BRACKET);
            })
            .skip('}', [&]()
            {
                add_token(Token::TOKEN_R_CURLY_BRACKET);
            });
        }

        if(lexer.current().is(BASE10_DIGITS) && lexer)
        {
            u32 line = lexer.get_line_number();
            u32 column = lexer.get_column_number();

            lexer_info.token_info.push_back({ .token = lexer_parse_number(lexer), .line_number = line, .column_number = column });
        }
        else if(!lexer.current().is(RESERVED_CHARS) && lexer)
        {
            u32 line = lexer.get_line_number();
            u32 column = lexer.get_column_number();

            lexer_info.token_info.push_back({ .token = lexer_parse_identifier(lexer), .line_number = line, .column_number = column });
        }
        else if(lexer.current().is(STRING_LITERIAL_DECLARATION_CHARS) && lexer)
        {
            u32 line = lexer.get_line_number();
            u32 column = lexer.get_column_number();

            lexer_info.token_info.push_back({ .token = lexer_parse_string_literial(lexer), .line_number = line, .column_number = column });
        }

        lexer.advance();
    }

    return lexer_info;
}
