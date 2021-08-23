#include "builder.h"

//
// Begins construction of string
//
struct StringBuilder sbstart() {
    return (struct StringBuilder) {
        .str = NULL,
        .size = 0,
        .capacity = 0
    };
}

static void assertf(int cond, const char *restrict fmt, ...) {
    if (!cond) {
        va_list vl;
        va_start(vl, fmt);
        fprintf(stderr, fmt, vl);
        va_end(vl);
        exit(-1);
    }
}

// 
// Returns length of formatter args
//
static int fmtlen(const char *restrict fmt, va_list vl){
    int size = vsnprintf(NULL, 0, fmt, vl);
    return size;
}

//
// Resizes
// 
static void resize(struct StringBuilder *self, size_t fmt_len) {
    if (self->size+fmt_len >= self->capacity) {
        self->capacity += fmt_len*2;
  //      printf("Capacity %ld Size %ld Fmtlen %ld\n", self->capacity, self->size, fmt_len);
        assertf(self->capacity > self->size, "Capacity doesn't exceed size: Capacity %ld vs Size %ld", self->capacity, self->size);

        if (self->str == NULL)
            self->str = malloc(self->capacity+1);
        else 
            self->str = realloc(self->str, self->capacity+1);
    }
}

//
// Printf into the string (va_list)
//
void vsbprintf(struct StringBuilder *self, const char *restrict fmt, va_list vl) {
    va_list lentmp;
    va_copy(lentmp, vl);
    int len = fmtlen(fmt, lentmp);
    resize(self, (size_t) len);

    int size = vsprintf(self->str+self->size, fmt, vl);

    assertf(size == len, "(StringBuilder: Warn) Size: %d != Len: %d :: \"%s\"\n", size, len, fmt);
    assertf(size >= 0, "(StringBuilder: Err) Vsprintf failed :: \"%s\"\n", fmt);
    self->size += (size_t) size;
    va_end(vl);
}

//
// Printf n-chars into the string (va_list)
//
void vsbnprintf(struct StringBuilder *self, size_t n, const char *restrict fmt, va_list vl) {
    va_list lentmp;
    va_copy(lentmp, vl);
    resize(self, n);

    int size = vsnprintf(self->str+self->size, n+1, fmt, vl);
    self->size += n;
  //  printf("Size is: %d\n", self->size);
    assertf(size >= 0, "(StringBuilder: Err) Vsnprintf failed :: \"%s\"\n", fmt);
    va_end(vl);
}

//
// Printf into the string
//
void sbprintf(struct StringBuilder *self, const char *restrict fmt, ...) {
    va_list vl;
    va_start(vl, fmt);
    vsbprintf(self, fmt, vl);
    va_end(vl);
}

//
// Printf n-chars into the string
//
void sbnprintf(struct StringBuilder *self, size_t n, const char *restrict fmt, ...) {
    va_list vl;
    va_start(vl, fmt);
    vsbnprintf(self, n, fmt, vl);
    va_end(vl);
}

//
// Returns the char pointer to the string you've made
// You must free the string after use
// 
char* sbend(struct StringBuilder *self) {
    return self->str;
}
