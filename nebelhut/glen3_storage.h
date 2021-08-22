#ifndef GLEN3_STORAGE_H
#define GLEN3_STORAGE_H

template <typename T>
struct Array {
    T *data;
    s64 count;
    s64 capacity;
    ArenaAllocator *arena;

    T &operator[](s64 idx) {
        assert(idx >= 0 && idx < this->count);
        return this->data[idx];
    }

    typename Slice<T>::Iterator begin() {
        Slice<T> slice = {.data = this->data, .count = this->count};
        return slice.begin();
    }

    typename Slice<T>::Iterator end() {
        Slice<T> slice = {.data = this->data, .count = this->count};
        return slice.end();
    }
};

template <typename T>
Array<T> array_alloc(ArenaAllocator *arena, s64 capacity) {
    Array<T> array;
    array.data = arena_alloc_slice<T>(arena, capacity).data;
    array.count = 0;
    array.capacity = capacity;
    return array;
}

template <typename T>
void array_clear(Array<T> *array) {
    array->count = 0;
}

template <typename T>
T *array_push(Array<T> *array) {
    if (array->count >= array->capacity && array->arena) {
        if (!array->capacity) array->capacity = 16;
        Array<T> copy = array_alloc<T>(array->arena, array->capacity * 2);
        copy.arena = array->arena;
        memcpy(copy.data, array->data, array->count * sizeof (T));
        copy.count = array->count;
        *array = copy;
    }

    assert(!array->arena || (array->arena && array->count < array->capacity));
    if (array->count < array->capacity) return array->data + array->count++;
    else assert(!"Capacity exceeded.");
}

template <typename T>
void array_push(Array<T> *array, T value) {
    *array_push(array) = value;
}

template<typename T>
void array_remove(Array<T> *array, s64 i) {
    Slice<T> s = {.data = array->data, .count = array->count};
    slice_remove(&s, i);
    array->count -= 1;
}

template <typename T>
T array_pop(Array<T> *array) {
    if (array->count == 0) {
        assert(!"Empty array.");
        return {};
    }
    return array->data[--array->count];
}

template <typename T>
s64 array_size_in_bytes(Array<T> *array) {
    return array->count * sizeof (T);
}

template <typename T>
Slice<T> array_slice(Array<T> *array) {
    Slice<T> slice{array->data, array->count};
    return slice;
}

template <typename T>
struct BucketArray {
    struct Bucket {
        s64 count;
        Bucket *next;
        T *data;
    };

    struct Iterator {
        Bucket *bucket;
        s64 index;

        T &operator*() {
            return this->bucket->data[this->index];
        }

        Iterator &operator++() {
            if (!this->bucket) {
                this->index = -1;
                return *this;
            }

            this->index += 1;
            if (this->index >= this->bucket->count) {
                this->bucket = this->bucket->next;
                this->index = 0;
            }
            return *this;
        }

        bool operator!=(const Iterator &b) {
            return this->bucket != b.bucket || this->index != b.index;
        }
    };

    Bucket *head;
    Bucket *tail;
    s64 bucket_size;
    ArenaAllocator *arena;

    Iterator begin() {
        if (this->head && this->head->count == 0)
            return this->end();
        return {.bucket = this->head, .index = 0};
    }

    Iterator end() {
        return {.bucket = nullptr, .index = 0};
    }
};

template <typename T>
void bucket_array_reset(BucketArray<T> *array) {
    array->head = NULL;
    array->tail = NULL;
}

// Throw away all buckets except the first one which is cleared of all elements but
// is kept allocated. If the corresponding arena is reset between a call to this
// and the next push, you need to call bucket_array_reset instead (which removes
// all buckets entirely).
template <typename T>
void bucket_array_clear_to_first_bucket(BucketArray<T> *array) {
    if (!array->head) return;

    array->head->count = 0;
    array->head->next = NULL;
    array->tail = array->head;
}

// Clear all buckets of their contents but keep them allocated.
template <typename T>
void bucket_array_empty_buckets(BucketArray<T> *array) {
    typename BucketArray<T>::Bucket *cur = array->head;
    while (cur) {
        cur->count = 0;
        cur = cur->next;
    }
    array->tail = array->head;
}

template <typename T>
typename BucketArray<T>::Bucket *bucket_array_alloc_bucket(BucketArray<T> *array) {
    assert(array->arena);
    if (!array->bucket_size) array->bucket_size = 32;
    typename BucketArray<T>::Bucket *bucket = arena_alloc<typename BucketArray<T>::Bucket>(array->arena);
    bucket->data = arena_alloc_many<T>(array->arena, array->bucket_size);
    bucket->count = 0;
    bucket->next = NULL;
    return bucket;
}

// Ensure that there is place for count contiguous items in the tail bucket.
template<typename T>
void bucket_array_ensure(BucketArray<T> *array, s64 count) {
    if (!array->bucket_size) array->bucket_size = count < 32 ? 32 : count;
    assert(count < array->bucket_size);
    if (array->tail) {
        if (array->tail->count + count >= array->bucket_size) {
            if (!array->tail->next)
                array->tail->next = bucket_array_alloc_bucket(array);
            array->tail = array->tail->next;
        }
    } else {
        array->tail = bucket_array_alloc_bucket(array);
        assert(!array->head);
        array->head = array->tail;
    }
}

template <typename T>
T *bucket_array_push(BucketArray<T> *array) {
    bucket_array_ensure(array, 1);
    T *result = array->tail->data + array->tail->count++;
    memory_fill(result, sizeof *result, 0);
    return result;
}

template <typename T>
T *bucket_array_push(BucketArray<T> *array, T value) {
    T *elem = bucket_array_push(array);
    copy_memory(elem, &value, sizeof value);
    return elem;
}

template <typename T>
s64 bucket_array_count(BucketArray<T> array) {
    s64 count = 0;
    typename BucketArray<T>::Bucket *cur = array.head;
    while (cur) {
        count += cur->count;
        cur = cur->next;
    }
    return count;
}

template <typename T>
T &bucket_array_get(BucketArray<T> array, s64 index) {
    typename BucketArray<T>::Bucket *cur = array.head;
    while (cur) {
        if (index < cur->count) break;
        index -= cur->count;
        cur = cur->next;
    }
    if (!cur) assert(!"Out-of-range index for array.");
    assert(index >= 0 && index < cur->count);
    return cur->data[index];
}

template <typename T>
T *bucket_array_get_if_contains(BucketArray<T> array, s64 index) {
    typename BucketArray<T>::Bucket *cur = array.head;
    while (cur) {
        if (index < cur->count) break;
        index -= cur->count;
        cur = cur->next;
    }
    if (!cur) return NULL;
    assert(index >= 0 && index < cur->count);
    return cur->data + index;
}

template <typename T>
s64 bucket_array_find(BucketArray<T> array, T *elem) {
    typename BucketArray<T>::Bucket *cur = array.head;
    s64 idx = 0;
    while (cur) {
        if (elem >= cur->data && elem < cur->data + cur->count) return idx + elem - cur->data;
        idx += cur->count;
        cur = cur->next;
    }
    return -1;
}

template <typename T>
bool bucket_array_contains(BucketArray<T> array, T *elem) {
    return bucket_array_find(array, elem) != -1;
}

template <typename T>
void bucket_array_remove(BucketArray<T> *array, T *elem) {
    typename BucketArray<T>::Bucket *cur = array->head;
    while (cur) {
        if (elem >= cur->data && elem < cur->data + cur->count) break;
        cur = cur->next;
    }
    if (!cur) {
        assert(!"Element was not found in bucket array.");
        return;
    }

    s64 index = elem - cur->data;
    if (index + 1 < cur->count)
        memory_copy((u8 *)(cur->data + index), {.data = (u8 *)(cur->data + index + 1), .count = (cur->count - index - 1) * sizeof (T)});
    cur->count -= 1;
}

// Return the offset of ptr as if the contents of array were contiguos. Offset is in bytes.
// If ptr is not contained within array, return -1.
template<typename T>
s64 bucket_array_find_offset(BucketArray<T> array, void *ptr) {
    auto *cur = array.head;
    s64 offset = 0;
    while (cur) {
        if (ptr >= cur->data && ptr < cur->data + cur->count)
            return offset + ((u8 *)ptr - (u8 *)cur->data);
        offset += cur->count;
        cur = cur->next;
    }
    return -1;
}

template<typename T>
Slice<T> bucket_array_linearize(BucketArray<T> array, ArenaAllocator *arena) {
    Slice<T> result = arena_alloc_slice<T>(arena, bucket_array_count(array));
    s64 idx = 0;
    auto *cur = array.head;
    while (cur) {
        copy_memory(result.data + idx, cur->data, cur->count * sizeof *cur->data);
        idx += cur->count;
        cur = cur->next;
    }
    return result;
}

static void concat_write(BucketArray<u8> *array, void *pdata, s64 size) {
    u8 *data = (u8 *)pdata;
    if (!array->tail) {
        array->head = bucket_array_alloc_bucket(array);
        array->tail = array->head;
    }
    auto *cur = array->tail;
    while (size > 0) {
        s64 n = size;
        if (n > array->bucket_size - cur->count) n = array->bucket_size - cur->count;
        copy_memory(cur->data + cur->count, data, n);
        data += n;
        cur->count += n;
        size -= n;
        if (!cur->next) {
            cur->next = bucket_array_alloc_bucket(array);
            array->tail = cur->next;
        }
        cur = cur->next;
    }
}

FORCEINLINE static void inline concat_write(BucketArray<u8> *array, Slice<u8> data) {
    concat_write(array, data.data, data.count);
}

FORCEINLINE static inline void concat_bytes(BucketArray<u8> *array, u8 b0) {
    bucket_array_push(array, b0);
}
FORCEINLINE static inline void concat_bytes(BucketArray<u8> *array, u8 b0, u8 b1) {
    bucket_array_push(array, b0);
    bucket_array_push(array, b1);
}
FORCEINLINE static inline void concat_bytes(BucketArray<u8> *array, u8 b0, u8 b1, u8 b2) {
    bucket_array_push(array, b0);
    bucket_array_push(array, b1);
    bucket_array_push(array, b2);
}
FORCEINLINE static inline void concat_bytes(BucketArray<u8> *array, u8 b0, u8 b1, u8 b2, u8 b3) {
    bucket_array_push(array, b0);
    bucket_array_push(array, b1);
    bucket_array_push(array, b2);
    bucket_array_push(array, b3);
}

// Return a pointer to where the next byte will be written.
// Ensures that there is enough space for count contiguous bytes.
FORCEINLINE static inline u8 *concat_cursor(BucketArray<u8> *array, s64 count = 1) {
    bucket_array_ensure(array, count);
    return array->tail->data + array->tail->count;
}

#endif // GLEN3_STORAGE_H