#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

//
// StringBuilder, will make cool strings with a very easy to use interface
//
struct StringBuilder {
    char *str;
    size_t capacity;
    size_t size;
};

#ifndef STRING_BUILDER_DONT_CREATE_SB_T_TYPEDEF
    typedef struct StringBuilder sb_t;
#endif
//
// Begins construction of string
//
struct StringBuilder sbstart();

//
// Printf into the string (va_list)
//
void vsbprintf(struct StringBuilder *self, const char *restrict fmt, va_list vl);

//
// Printf n-chars into the string (va_list)
//
void vsbnprintf(struct StringBuilder *self, size_t n, const char *restrict fmt, va_list vl);

//
// Printf into the string
//
void sbprintf(struct StringBuilder *self, const char *restrict fmt, ...) __attribute__((format(printf, 2, 3)));

//
// Printf n-chars into the string
//
void sbnprintf(struct StringBuilder *self, size_t n, const char *restrict fmt, ...) __attribute__((format(printf, 3, 4)));

//
// Returns the char pointer to the string you've made
// You must free the string after use
// 
char* sbend(struct StringBuilder *self);
