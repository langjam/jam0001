#ifndef GLEN3_BASE_H
#define GLEN3_BASE_H

// You can define GLEN3_NO_LIBC before including to use custom implementations of some functions.
// Otherwise we call to libc for performing fast memory copies/fills/compares.

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#ifndef GLEN3_NO_LIBC
#include <string.h>
#endif

#include <stdio.h>
#include <sys/mman.h>
#include <time.h>

typedef  uint8_t  u8;
typedef   int8_t  s8;
typedef uint16_t u16;
typedef  int16_t s16;
typedef uint32_t u32;
typedef  int32_t s32;
typedef uint64_t u64;
typedef  int64_t s64;
typedef    float f32;
typedef   double f64;

#define ARRAY_COUNT(a) s64(sizeof (a) / sizeof *(a))
#define UNIMPLEMENTED() assert(!"Not implemented")
#define UNREACHABLE() assert(!"Unreachable")
#define FORCEINLINE __attribute__((always_inline))

FORCEINLINE static inline void *os_alloc(s64 size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
}

FORCEINLINE static inline void os_dealloc(void *p, s64 size) {
    munmap(p, size);
}

static inline s64 os_get_monotonic_us() {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return 1000000 * t.tv_sec + t.tv_nsec / 1000;
}

template <typename T>
struct Slice {
    T *data;
    s64 count;

    struct Iterator {
        T *data;
        s64 index;

        T &operator*() {
            return this->data[this->index];
        }

        Iterator &operator++() {
            this->index += 1;
            return *this;
        }

        bool operator!=(const Iterator &b) {
            return this->index != b.index;
        }
    };

    T &operator[](s64 index) {
        assert(index >= 0 && index < this->count);
        return this->data[index];
    }

    Iterator begin() {
        return {.data = this->data, .index = 0};
    }

    Iterator end() {
        return {.data = this->data, .index = this->count};
    }
};

template<typename T>
bool operator==(const Slice<T> &a, const Slice<T> &b) {
    if (a.count != b.count) return false;

#ifdef GLEN3_NO_LIBC
    for (s64 i = 0; i < a.count; i++)
        if (a.data[i] != b.data[i])
            return false;
    return true;
#else
    return memcmp(a.data, b.data, a.count * sizeof (T)) == 0;
#endif
}

template<typename T>
bool operator!=(const Slice<T> &a, const Slice<T> &b) {
    if (a.count != b.count) return true;

    for (s64 i = 0; i < a.count; i++)
        if (a.data[i] != b.data[i])
            return true;

    return false;
}

template<typename T>
void slice_advance(Slice<T> *slice, s64 n) {
    if (n > slice->count) n = slice->count;
    slice->data += n;
    slice->count -= n;
}

template<typename T>
bool slice_skip(Slice<T> *s, Slice<T> p) {
    if (slice_has_prefix(*s, p)) {
        slice_advance(s, p.count);
        return true;
    } else {
        return false;
    }
}

static inline bool slice_skip(Slice<u8> *s, u8 ch) {
    if (!s->count || s->data[0] != ch) return false;
    slice_advance(s, 1);
    return true;
}

static inline Slice<u8> str_slice(const char *str) {
    if (!str) return (Slice<u8>){};
    s64 size = 0;
#ifdef GLEN3_NO_LIBC
    while (str[size])
        size += 1;
#else
    size = strlen(str);
#endif
    return (Slice<u8>){.data = (u8 *)str, .count = size};
}

#define lit_slice(s) ((Slice<u8>){.data = (u8 *)(s), .count = sizeof (s) - 1})
#define mem_slice(m) ((Slice<u8>){.data = (u8 *)(m), .count = sizeof *(m)})
#define STRFMT(s) (s32)(s).count, (s).data

template <typename T>
void copy_slice(T *dest, Slice<T> src) {
#ifdef GLEN3_NO_LIBC
    if (dest > src.data) {
        dest += src.count;
        src.data += src.count;
        while (src.count--) *--dest = *--src.data;
    } else {
        while (src.count--) *dest++ = *src.data++;
    }
#else
    memmove(dest, src.data, src.count * sizeof (T));
#endif
}

template <typename T>
FORCEINLINE static inline void copy_slice(Slice<T> dest, Slice<T> src) {
    assert(dest.count >= src.count);
    copy_slice(dest.data, src);
}

template<typename T>
FORCEINLINE void slice_remove(Slice<T> *slice, s64 index) {
    assert(index >= 0 && index < slice->count);
    Slice<T> src = {.data = slice->data + index + 1, .count = slice->count - index - 1};
    copy_slice(slice->data + index, src);
    slice->count -= 1;
}

template<typename T>
FORCEINLINE static inline bool slice_has_prefix(Slice<T> data, Slice<T> prefix) {
    Slice<u8> d = {.data = data.data, .count = prefix.count};
    return d == prefix;
}

FORCEINLINE static inline void copy_memory(void *pdest, void *psrc, s64 size) {
    copy_slice((u8 *)pdest, {.data = (u8 *)psrc, .count = size});
}

template<typename T>
FORCEINLINE bool memory_has_suffix(Slice<T> data, Slice<T> suffix) {
    s64 c = data.count - suffix.count;
    Slice<u8> d = {.data = data.data + c, .count = c};
    return d == suffix;
}

template<typename T>
static s64 memory_find(Slice<T> haystack, Slice<T> needle) {
    for (s64 i = 0; i <= haystack.count - needle.count; i++) {
        bool found = true;
        for (s64 j = 0; j < needle.count; j++)
            if (haystack.data[i + j] != needle[j]) {
                found = false;
                break;
            }
        if (found) return i;
    }
    return -1;
}

template<typename T>
static s64 memory_find_backwards(Slice<T> haystack, Slice<T> needle) {
    s64 i = haystack.count - needle.count;
    while (i >= 0) {
        bool found = true;
        for (s64 j = 0; j < needle.count; j++)
            if (haystack.data[i + j] != needle[j]) {
                found = false;
                break;
            }
        if (found) return i;
        i -= 1;
    }
    return -1;
}

template<typename T>
FORCEINLINE inline void memory_fill(Slice<T> dest, T value) {
    for (s64 i = 0; i < dest.count; i++)
        dest[i] = value;
}

FORCEINLINE static inline void memory_fill(void *pdest, s64 size, u8 value) {
#ifdef GLEN3_NO_LIBC
    u8 *dest = (u8 *)pdest;
    for (s64 i = 0; i < size; i++)
        dest[i] = value;
#else
    memset(pdest, value, size);
#endif
}

template<>
FORCEINLINE inline void memory_fill(Slice<u8> dest, u8 value) {
    memory_fill(dest.data, dest.count, value);
}

static inline bool memory_is_zero(void *m, s64 size) {
    for (s64 i = 0; i < size; i++)
        if (((u8 *)m)[i]) return false;
    return true;
}

static inline void skip_space(Slice<u8> *s) {
    while (s->count && isspace(s->data[0])) {
        s->data += 1;
        s->count -= 1;
    }
}

static inline void trim(Slice<u8> *s) {
    skip_space(s);
    while (s->count && isspace(s->data[s->count - 1]))
        s->count -= 1;
}

static inline u8 string_peek(Slice<u8> s, s64 n = 0) {
    if (s.count < n) return 0;
    return s.data[n];
}

static inline bool read_u8(Slice<u8> *s, u8 *val) {
    if (!s->count) return false;
    *val = s->data[0];
    slice_advance(s, 1);
    return true;
}

static inline bool read_u32le(Slice<u8> *s, u32 *val) {
    if (s->count < 4) return false;
    *val = 0;
    *val |= (u32)s->data[0];
    *val |= (u32)s->data[1] << 8;
    *val |= (u32)s->data[2] << 16;
    *val |= (u32)s->data[3] << 24;
    slice_advance(s, 4);
    return true;
}

static inline bool read_u32be(Slice<u8> *s, u32 *val) {
    if (s->count < 4) return false;
    *val = 0;
    *val |= (u32)s->data[0] << 24;
    *val |= (u32)s->data[1] << 16;
    *val |= (u32)s->data[2] << 8;
    *val |= (u32)s->data[3];
    slice_advance(s, 4);
    return true;
}

struct ArenaAllocator {
    struct Block {
        s64 size;
        s64 used;
        Block *prev;
        u8 data[];
    };

    Block *tail;
    s64 pagesize;
    s64 total_allocated;
};

// Memory allocated through arena_alloc_raw is always zeroed.
static void *arena_alloc_raw(ArenaAllocator *arena, s64 size, s64 align) {
    assert(align > 0);
    assert((align & (align - 1)) == 0);

    if (!arena->pagesize) arena->pagesize = 1024 * 1024;

    if (arena->tail)
        arena->tail->used += (align - ((uintptr_t)(arena->tail->data + arena->tail->used) & (align - 1))) & (align - 1);

    if (!arena->tail || arena->tail->used + size > arena->tail->size) {
        s64 alloc_size = arena->pagesize - sizeof (ArenaAllocator::Block);
        if (size > alloc_size) alloc_size = size;
        ArenaAllocator::Block *block = (ArenaAllocator::Block *)(os_alloc(sizeof *block + alloc_size));
        block->size = alloc_size;
        block->prev = arena->tail;
        block->used = (align - ((uintptr_t)block->data & (align - 1))) & (align - 1);
        arena->tail = block;
    }
    assert(arena->tail->used + size <= arena->tail->size);

    void *result = arena->tail->data + arena->tail->used;
    arena->tail->used += size;
    arena->total_allocated += size;
    return result;
}


// Do macros for arena_alloc, arena_alloc_many and arena_alloc_slice? Reduce on template bloat?
template <typename T>
T *arena_alloc(ArenaAllocator *arena, s64 align = alignof (T)) {
    return (T *)arena_alloc_raw(arena, sizeof (T), align);
}

template <typename T>
T *arena_alloc_many(ArenaAllocator *arena, s64 count, s64 align = alignof (T)) {
    return (T *)arena_alloc_raw(arena, sizeof (T) * count, align);
}

template <typename T>
Slice<T> arena_alloc_slice(ArenaAllocator *arena, s64 count) {
    Slice<T> slice;
    slice.data = (T *)arena_alloc_raw(arena, sizeof (T) * count, alignof (T));
    slice.count = count;
    return slice;
}

// Clears the arena but keeps the first page allocated (if there is one).
static void arena_reset(ArenaAllocator *arena) {
    if (!arena->tail) return;

    while (arena->tail->prev) {
        auto *block = arena->tail;
        arena->tail = block->prev;
        os_dealloc(block, sizeof *block + block->size);
    }
    memory_fill(arena->tail->data, arena->tail->size, 0);
    arena->tail->used = 0;
    arena->total_allocated = 0;
}

// Completely deallocates arena.
static inline void arena_dealloc(ArenaAllocator *arena) {
    arena_reset(arena);
    if (arena->tail)
        os_dealloc(arena->tail, sizeof *arena->tail + arena->tail->size);
    arena->tail = NULL;
    arena->total_allocated = 0;
}

// What about ArenaMarker? I don’t seem to use it, although it seems useful.

template<typename T>
T *copy_to_arena(ArenaAllocator *arena, T *val) {
    T *result = arena_alloc<T>(arena);
    copy_memory(result, val, sizeof *val);
    return result;
}

template<typename T>
T *copy_to_arena(ArenaAllocator *arena, T val) {
    T *result = arena_alloc<T>(arena);
    *result = val;
    return result;
}

template<typename T>
Slice<T> copy_slice_to_arena(ArenaAllocator *arena, Slice<T> src) {
    Slice<T> dest = arena_alloc_slice<T>(arena, src.count);
    copy_slice(dest, src);
    return dest;
}

static inline char *to_c_string(ArenaAllocator *arena, Slice<u8> src) {
    u8 *result = arena_alloc_many<u8>(arena, src.count + 1);
    copy_slice(result, src);
    result[src.count] = '\0';
    return (char *)result;
}

static inline u32 decode_utf8_scalar_slow(Slice<u8> buffer, int *advance) {
    assert(buffer.count);

    u32 result = 0;
    int remaining;
    if ((buffer[0] & 0x80) == 0) {
        remaining = 0;
        result = buffer[0] & 0x7f;
    } else if ((buffer[0] & 0b11100000) == 0b11000000) {
        remaining = 1;
        result = buffer[0] & 0b00011111;
    } else if ((buffer[0] & 0b11110000) == 0b11100000) {
        remaining = 2;
        result = buffer[0] & 0b00001111;
    } else if ((buffer[0] & 0b11111000) == 0b11110000) {
        remaining = 3;
        result = buffer[0] & 0b00000111;
    } else {
        // buffer does not point at a valid UTF8 string.
        if (advance) *advance = 1;
        return 0xfffd;
    }

    for (int i = 0; i < remaining; i++) {
        if (i + 1 >= buffer.count) {
            if (advance) *advance = i + 1;
            return 0xfffd;
        }

        if ((buffer[i + 1] & 0b11000000) != 0b10000000) {
            // Not a valid continuation byte.
            if (advance) *advance = i + 1;
            return 0xfffd;
        }
        result = (result << 6) | (buffer[i + 1] & 0b00111111);
    }

    if (remaining + 1 < buffer.count && (buffer[remaining + 1] & 0b11000000) == 0b10000000) {
        // Spurious continuation byte.
        // Actually, we know that this invalid sequence is at least
        // remaining + 2 bytes long but we want to keep advance
        // between 1 and 4.
        if (advance) *advance = remaining + 1;
        return 0xfffd;
    }

    if ((result >= 0xd800 && result < 0xe000) || result >= 0x110000) {
        // Surrogate code points and code points beyond the specified
        // range of Unicode are invalid.
        if (advance) *advance = remaining + 1;
        return 0xfffd;
    }

    if (advance) *advance = remaining + 1;
    return result;
}

FORCEINLINE static inline u32 decode_utf8_scalar(Slice<u8> buffer, int *advance) {
	assert(buffer.count);
	if ((buffer.data[0] & 0x80) == 0) {
		if (advance) *advance = 1;
		return buffer.data[0];
	}
	return decode_utf8_scalar_slow(buffer, advance);
}

static inline char *sprintf_arena(ArenaAllocator *arena, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    s32 size = vsnprintf(NULL, 0, fmt, args);
    va_end(args);
    char *buf = arena_alloc_many<char>(arena, size + 1);
    va_start(args, fmt);
    vsnprintf(buf, size + 1, fmt, args);
    va_end(args);
    return buf;
}

#define tprint(...) sprintf_arena(&frame_arena, ## __VA_ARGS__)

static inline bool read_entire_file(const char *filename, ArenaAllocator *arena, Slice<u8> *out) {
    FILE *in = fopen(filename, "rb");
    if (!in) return false;
    fseek(in, 0, SEEK_END);
    s64 size = ftell(in);
    fseek(in, 0, SEEK_SET);
    u8 *buffer = arena_alloc_many<u8>(arena, size);
    s64 read = 0;
    while (read < size) {
        size_t n = fread(buffer + read, 1, size - read, in);
        if (n == 0 && ferror(in)) return false;
        else if (n == 0) break;
        else read += n;
    }
    fclose(in);
    if (out) *out = {.data = buffer, .count = read};
    return true;
}

static inline Slice<u8> read_entire_file(const char *filename, ArenaAllocator *arena) {
    Slice<u8> out;
    if (!read_entire_file(filename, arena, &out)) {
        char message[1024];
        snprintf(message, sizeof message, "read_entire_file %s: open", filename);
        perror(message);
        return {};
    }
    return out;
}

#endif // GLEN3_BASE_H
