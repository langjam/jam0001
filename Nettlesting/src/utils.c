#include "utils.h"
bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\v';
}
bool is_newline(char c)
{
    return c == '\n' || c == 0xD;
}
bool is_identifier(char c, long index)
{
    if (index == 0)
        return c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    else
        return c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9');
}
bool is_quote(char c)
{
    return c == '"' || c == '\'';
}
bool is_single_operator(char c)
{
    return c == '+' || c == '-' || c == '*' || c == '/' || c == '^' || c == '!' || c == '(' || c == ')' || c == '#' || c == '%';
}
bool is_multi_operator(char first, char current, int index)
{
    if (index == 0)
    {
        return current == '<' || current == '>' || current == '&' || current == '|' || current == ':';
    } else if (index == 1) 
    {
        if (first == '<') return current == '=' || current == '<';
        if (first == '>') return current == '=' || current == '>';
        if (first == ':') return current == '=';
        return (current == first) && (current == '&' || current == '|');
    }
    return false;
}
bool is_hex(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}
bool is_dec(char c)
{
    return c >= '0' && c <= '9';
}
bool is_oct(char c)
{
    return c >= '0' && c <= '7';
}
bool is_bin(char c)
{
    return c == '0' || c == '1';
}
