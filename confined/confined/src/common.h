#pragma once

#include <stdint.h>
#include <stddef.h>
#include <string>
#include <iostream>

using u8 = uint8_t;
using i8 = int8_t;
using u16 = uint16_t;
using i16 = int16_t;
using u32 = uint32_t;
using i32 = int32_t;
using u64 = uint64_t;
using i64 = int64_t;
#ifdef __GNUC__
using i128 = __int128_t;
#endif

#ifdef USE_WCHAR
using char_t = wchar_t;
#else
using char_t = char;
#endif

using str_t = std::basic_string<char_t>;

#ifdef USE_WCHAR
#define S(__string) L##__string
#define PSOUT() std::wcout
#define PSERR() std::wcerr
#define SSTREAM std::wstringstream
#define OFSTREAM std::wofstream
#define IFSTREAM std::wifstream
#define OSSTREAM std::wostringstream

static str_t CONVERT_STR(const char* str)
{
    return str_t(&str[0], &str[std::string(str).size()]);
}

#else
#define S(__string) __string
#define PSOUT() std::cout
#define PSERR() std::cerr
#define SSTREAM std::stringstream
#define OFSTREAM std::ofstream
#define IFSTREAM std::ifstream
#define OSSTREAM std::ostringstream

static str_t CONVERT_STR(const char* str)
{
    return str_t(str);
}
#endif
