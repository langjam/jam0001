#include "lang.h"
#include "parser.h"
#include "runtime.h"
#include <fstream>
#include <sstream>

static ParserInfo lang_init(str_t source, const str_t& filename, bool verbose)
{
    LexerInfo lexed = lang_lex(source);
    if(verbose) lexed.dump();

    ParserInfo parser_info = lang_parse(lexed, filename);

    if(verbose)
    {
        for(auto& node : parser_info.nodes)
        {
            PSOUT() << node->to_string() << std::endl;
        }

        PSOUT() << "\n";
    }

    return parser_info;
}

void lang_execute_from_file(const char* file_location, bool verbose)
{
    str_t source;

    IFSTREAM file;
    file.open(file_location);

    if(!file.is_open())
    {
        PSERR() << "File does not exist!" << std::endl;

        exit(EXIT_FAILURE);
    }

    OSSTREAM ss;
    ss << file.rdbuf();

    source = ss.str();

    file.close();

    auto parser_info = lang_init(source, CONVERT_STR(file_location), verbose);

    lang_start_runtime(parser_info);
}

void lang_execute(str_t source, bool verbose)
{
    auto parser_info = lang_init(source, S("<unknown>"), verbose);

    lang_start_runtime(parser_info);
}
