#pragma once

#include "common.h"
#include <string>
#include <functional>

class Lexer
{
private:
    using char_type = char_t;
    using string_type = str_t;

    string_type _source;
    u32 _index = 0;
    u32 _line_number = 1;
    u32 _column_number = 0;

public:
    class StringIter
    {
    private:
        const string_type* _s;
        u32 _i = 0;

        StringIter(const string_type& s, u32 i)
        :
        _s(&s),
        _i(i)
        {
        }

        StringIter()
        :
        _s(nullptr),
        _i(-1)
        {
        }

        char_type get(int i) const
        {
            if(_s == nullptr || (i >= _s->size() || i < 0)) return '\0';

            return _s->at(i);
        }

    public:
        operator const char_type() const
        {
            return get(_i);
        }

        StringIter operator++(int)
        {
            StringIter temp = *this;

            ++(*this);

            return temp;
        }

        StringIter& operator++()
        {
            _i++;

            return *this;
        }

        StringIter operator--(int)
        {
            StringIter temp = *this;

            --(*this);

            return temp;
        }

        StringIter& operator--()
        {
            _i--;

            return *this;
        }

        bool operator==(const StringIter& rhs) const
        {
            return rhs._i == _i && rhs._s == _s;
        }

        bool operator==(const string_type& rhs) const
        {
            if(get(_i) == '\0') { return false; }

            string_type substr = _s->substr(_i);

            return substr.rfind(rhs, 0) == 0;
        }

        bool is(const string_type& chars) const
        {
            if(get(_i) == '\0') { return false; }

            for(char_t c : chars)
            {
                if(get(_i) == c)
                {
                    return true;
                }
            }

            return false;
        }

        bool is_not(const string_type& chars) const
        {
            return is(chars) == false;
        }

        bool operator!=(const StringIter& rhs) const
        {
            return rhs._i != _i || rhs._s != _s;
        }

        const char_type operator*() const
        {
            return get(_i);
        }

        friend Lexer;
    };

    Lexer(const string_type& s)
    :
    _source(s)
    {
    }

    StringIter current()
    {
        return StringIter(_source, _index);
    }

    StringIter peek(i32 how_many = 1)
    {
        return StringIter(_source, _index + how_many);
    }

    const StringIter current() const
    {
        return StringIter(_source, _index);
    }

    const StringIter peek(i32 how_many = 1) const
    {
        return StringIter(_source, _index + how_many);
    }

    StringIter advance(i32 how_many = 1)
    {
        for(int i = 0; i < how_many; i++)
        {
            _index++;

            if(current() == '\n')
            {
                _column_number = 0;
                _line_number++;
            }
            else
            {
                _column_number++;
            }
        }

        return StringIter(_source, _index);
    }

    u32 get_line_number()
    {
        return _line_number;
    }

    u32 get_column_number()
    {
        return _column_number;
    }

    Lexer& skip(char_t c, std::function<void()> callback)
    {
        while(current() == c)
        {
            callback();

            advance();
        }

        return *this;
    }

    Lexer& skip(const str_t& s, std::function<void()> callback)
    {
        while(current() == s)
        {
            callback();

            advance(s.size());
        }

        return *this;
    }

    Lexer& skip(char_t c)
    {
        while(current() == c)
        {
            advance();
        }

        return *this;
    }

    Lexer& skip(const str_t& s)
    {
        while(current() == s)
        {
            advance(s.size());
        }

        return *this;
    }

    StringIter begin()
    {
        return StringIter(_source, 0);
    }

    StringIter end()
    {
        return StringIter(_source, _source.size() + 1);
    }

    operator bool() const
    {
        return current() != '\0';
    }
};
