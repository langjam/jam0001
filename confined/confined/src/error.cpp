#include "error.h"

void lang_print_error(const Error& error)
{
    str_t prompt;
    if(error.level == ERROR_LEVEL_FAIL) prompt = S("\x1B[31merror: \033[0m");
    else if(error.level == ERROR_LEVEL_INFO) prompt = S("\x1B[34info: \033[0m");
    else if(error.level == ERROR_LEVEL_WARNING) prompt = S("\x1B[33warning: \033[0m");

    PSERR() << error.source_file << ":" << error.line_number << ":" << error.column_number << " " << prompt << error.error_message << std::endl;
}
