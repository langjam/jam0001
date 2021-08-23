#pragma once

#include "common.h"

enum ErrorLevel
{
    ERROR_LEVEL_FAIL,
    ERROR_LEVEL_WARNING,
    ERROR_LEVEL_INFO
};

struct Error
{
    ErrorLevel level;
    str_t error_message = S("unknown error");
    str_t source_file = S("unknown");
    u32 line_number = 0;
    u32 column_number = 0;
};

void lang_print_error(const Error& error);
