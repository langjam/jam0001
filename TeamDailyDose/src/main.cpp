#include "Lexer.h"
#include "Parser.h"
#include <AK/Assertions.h>
#include <AK/Format.h>
#include <LibCore/ArgsParser.h>
#include <LibCore/File.h>

int dump_lexer(Lexer& lexer);
int dump_parser(Parser& parser);
int format_input(Parser& parser);
int execute_file(Parser& parser);

int dump_lexer(Lexer& lexer)
{
    while (true) {
        auto maybe_token = lexer.next_token();
        if (maybe_token.is_error()) {
            warnln("parse error: {} at {}", maybe_token.error(), lexer.location());
            return 1;
        }

        auto token = maybe_token.release_value();
        if (token.type() == TokenType::Eof) {
            dbgln("EOF!");
            break;
        }

        if (token.type() == TokenType::Comment) {
            outln("{} Comment \"{}\"", lexer.location(), token.string());
        } else if (token.type() == TokenType::Identifier) {
            outln("{} Identifier \"{}\"", lexer.location(), token.string());
        } else {
            outln("{} {}", lexer.location(), token_to_string(token.type()));
        }
    }

    return 0;
}

int dump_parser(Parser& parser)
{
    auto maybe_statement_list = parser.parse_statement_list(true);
    if (maybe_statement_list.is_error()) {
        outln("{} at {}", maybe_statement_list.error(), parser.location());
        return 1;
    }

    auto statement_list = maybe_statement_list.release_value();
    statement_list->dump();

    return 0;
}

int format_input(Parser& parser)
{
    auto maybe_statement_list = parser.parse_statement_list(true);
    if (maybe_statement_list.is_error()) {
        outln("{} at {}", maybe_statement_list.error(), parser.location());
        return 1;
    }

    auto statement_list = maybe_statement_list.release_value();
    statement_list->print();

    return 0;
}

int execute_file(Parser& parser)
{
    auto maybe_statement_list = parser.parse_statement_list(true);
    if (maybe_statement_list.is_error()) {
        outln("{} at {}", maybe_statement_list.error(), parser.location());
        return 1;
    }

    auto statement_list = maybe_statement_list.release_value();
    auto root_scope = Scope::construct();
    (void)statement_list->evaluate(root_scope);
    if (root_scope->has_exception()) {
        warnln("fatal error: {}", root_scope->exception());
    }

    return 0;
}

int main(int argc, char** argv)
{
    String filename;
    bool dump_lexer_output = false;
    bool dump_parser_output = false;
    bool format = false;

    Core::ArgsParser args_parser;
    args_parser.add_option(dump_lexer_output, "Dump lexer output", "dump-lexer", 'l');
    args_parser.add_option(dump_parser_output, "Dump parser output", "dump-parser", 'p');
    args_parser.add_option(format, "Format and print the given input", "format", 'f');
    args_parser.add_positional_argument(filename, "Input filename", "filename");
    args_parser.set_general_help("Interpreter for Team Daily Dose's entry for Langjam #0001.");

    args_parser.parse(argc, argv);

    auto maybe_file = Core::File::open(filename, Core::OpenMode::ReadOnly);
    if (maybe_file.is_error()) {
        warnln("Failed to open {}: {}", filename, maybe_file.error());
        return 1;
    }

    auto file = maybe_file.release_value();
    auto file_contents = file->read_all();

    Lexer lexer(file_contents);

    if (dump_lexer_output) {
        return dump_lexer(lexer);
    }

    Parser parser(lexer);

    if (dump_parser_output) {
        return dump_parser(parser);
    }

    if (format) {
        return format_input(parser);
    }

    return execute_file(parser);
}
