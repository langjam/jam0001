#pragma once

#include "common.h"

class Token
{
private:
    u32 _type;
    str_t _data;

public:
    #include "___token.h"

    enum : u32
    {
        TOKEN_MACRO(TOKEN_MACRO_GENERATE_ENUM)
    };

    #undef TOKEN_MACRO
    #undef TOKEN_MACRO_GENERATE_ENUM

    Token()
    :
    _type(TOKEN_EOF),
    _data(str_t())
    {
    }

    Token(u32 token_type)
    :
    _type(token_type),
    _data(str_t())
    {
    }

    Token(u32 token_type, const str_t& data)
    :
    _type(token_type),
    _data(data)
    {
    }

    void set_data(const str_t& data)
    {
        _data = data;
    }

    void set_type(u32 type)
    {
        _type = type;
    }

    const str_t& get_data() const
    {
        return _data;
    }

    str_t to_string() const;

    bool operator==(const Token& other) const
    {
        return _type == other._type;
    }

    bool operator!=(const Token& other) const
    {
        return _type != other._type;
    }

    bool is_number() const
    {
        return  _type == TOKEN_BINARY  ||
                _type == TOKEN_DECIMAL ||
                _type == TOKEN_HEXADECIMAL;
    }

    bool is_binary_op() const
    {
        return  _type == TOKEN_ADD      ||
                _type == TOKEN_SUBTRACT ||
                _type == TOKEN_MULTIPLY ||
                _type == TOKEN_DIVIDE   ||
                _type == TOKEN_EQUALS;
    }

    bool is_identifier() const
    {
        return  _type == TOKEN_IDENTIFIER;
    }

    bool is_string_literial() const
    {
        return  _type == TOKEN_STRING_LITERIAL;
    }
};
