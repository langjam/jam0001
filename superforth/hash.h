#pragma once

#ifndef HASH_H
#define HASH_H

#include <stdint.h>

const uint64_t hash_s(const char* str, const uint64_t len);
const uint64_t hash(const char* str);

const uint64_t combine_hash(uint64_t a, uint64_t b);

#endif // !HASH_H