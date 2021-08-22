#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uintptr_t inc(uintptr_t a) {
    return a + 1;
}

uintptr_t dec(uintptr_t a) {
    return a - 1;
}

void store(uintptr_t ptr, uintptr_t val) {
    *(uintptr_t*)(ptr) = val;
}

void storeb(uintptr_t ptr, uintptr_t val) {
    *(char*)(ptr) = (char)val;
}

uintptr_t add(uintptr_t a, uintptr_t b) {
    return a + b;
}

uintptr_t sub(uintptr_t a, uintptr_t b) {
    return a - b;
}

uintptr_t greater(uintptr_t a, uintptr_t b) {
    return a > b;
}

uintptr_t less(uintptr_t a, uintptr_t b) {
    return a < b;
}

uintptr_t deref(uintptr_t ptr) {
    return *(uintptr_t*)(ptr);
}

uintptr_t equal(uintptr_t x, uintptr_t y) {
    return x == y;
}

uintptr_t equalchar(uintptr_t x, uintptr_t y) {
    return (char)x == (char)y;
}

uintptr_t bnot(uintptr_t x) {
    return ~x;
}

uintptr_t not(uintptr_t x) {
    return !x;
}

uintptr_t and(uintptr_t x, uintptr_t y) {
    return x & y;
}

uintptr_t or(uintptr_t x, uintptr_t y) {
    return x | y;
}

uintptr_t xor(uintptr_t x, uintptr_t y) {
    return x ^ y;
}

uintptr_t bsprint(uintptr_t a) {
    return printf("%li", a);
}

uintptr_t putsnl(uintptr_t ptr) {
    return printf("%s\n", (char*)ptr);
}

uintptr_t readfile(uintptr_t path) {
    FILE* f = fopen((char*)path, "r");
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* str = malloc(sz + 1);
    fread(str, 1, sz, f);
    fclose(f);
    str[sz] = '\0';
    return (uintptr_t)str;
}

uintptr_t sizeptr(uintptr_t unused) {
    return sizeof(void*);
}
