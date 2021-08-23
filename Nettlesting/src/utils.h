#ifndef _UTILS_H_
#define _UTILS_H_
#include <stdbool.h>
extern bool is_whitespace(char c);
extern bool is_newline(char c);
extern bool is_identifier(char c, long index);
extern bool is_quote(char c);
extern bool is_single_operator(char c);
extern bool is_multi_operator(char first, char current, int index);
extern bool is_hex(char c);
extern bool is_dec(char c);
extern bool is_oct(char c);
extern bool is_bin(char c);

#endif