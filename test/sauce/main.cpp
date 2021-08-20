#include "parser.h"
#include <AK/Format.h>
#include <AK/StringView.h>
#include <errno.h>
#include <string.h>

static StringView g_program_name;

int print_help(bool as_failure = false)
{
    outln("{} v0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0", g_program_name);
    outln("  usage: {} <source_file>", g_program_name);
    outln("    <source_file> can also be `-` to read from stdin");
    outln("That's it.");
    return as_failure ? 1 : 0;
}

void parse_and_execute();

int main(int argc, char** argv)
{
    g_program_name = argv[0];
    if (argc == 1)
        return print_help();

    auto source_file = argv[1];
    if (source_file != "-"sv) {
        auto file = freopen(source_file, "r", stdin);
        if (!file) {
            warnln("Failed to open {}: {}", source_file, strerror(errno));
            return 1;
        }
    }

    Lexer lexer;
    for (;;) {
        auto maybe_token = lexer.next();
        if (maybe_token.is_error()) {
            warnln("Lex error: {} at {}:{}", maybe_token.error().error, maybe_token.error().where.line, maybe_token.error().where.column);
            break;
        }

        auto token = maybe_token.release_value();
        if (token.type == Token::Type::Eof)
            break;

        warnln("- {} ({})", token.text, to_underlying(token.type));
    }
    return 0;
}
