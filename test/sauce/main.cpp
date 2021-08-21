#include "parser.h"
#include <AK/Format.h>
#include <AK/Function.h>
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

Value lang$print(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    bool first = true;
    Function<void(Value const&)> print_value = [&](Value const& value) {
        value.value.visit(
            [](Empty) { out("<empty>"); },
            [](NonnullRefPtr<FunctionNode> const&) { out("<fn ref>"); },    // FIXME
            [](Type*) { out("<type ref>"); }, // FIXME
            [&print_value](NonnullRefPtr<CommentResolutionSet> const& rs) {
                out("<Comment resolution set: {{");
                auto first = true;
                for (auto& entry : rs->values) {
                    if (!first)
                        out(", ");
                    first = false;
                    print_value(entry);
                }
                out("}}>");
            },
            [](NativeFunctionType const& fnptr) { out("<fnptr at {:p}>", fnptr.fn); },
            [](auto const& value) { out("{}", value); });
    };
    for (auto& arg : args) {
        if (!first)
            out(" ");
        print_value(arg);
        first = false;
    }
    outln();
    return { Empty {} };
}

Value lang$sub(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    Variant<Empty, int> accumulator { Empty {} };
    auto append = [&](auto arg) {
        Variant<Empty, int> value { Empty {} };
        if constexpr (requires { arg.template has<int>(); }) {
            if (arg.template has<Empty>() || arg.template has<int>())
                value = arg.template downcast<Empty, int>();
        } else if constexpr (IsSame<decltype(arg), int>) {
            value = arg;
        }

        if (accumulator.template has<Empty>()) {
            accumulator = value;
        } else if (accumulator.template has<int>()) {
            if (value.template has<int>()) {
                accumulator = accumulator.template get<int>() - value.template get<int>();
            }
        }
    };
    for (auto& arg : args) {
        arg.value.visit(
            [&](Empty) { append(Empty {}); },
            [&](NonnullRefPtr<FunctionNode> const&) { append(String("<function>"sv)); },
            [&](Type*) { append(String("<type>"sv)); },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) {
                for (auto& entry : crs->values)
                    append(entry.value);
            },
            [&](NativeFunctionType const&) { append(String("<fn>"sv)); },
            [&](auto const& value) { append(value); });
    }
    return { move(accumulator).downcast<Empty, int, String, Type*, NonnullRefPtr<FunctionNode>, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType>() };
}

Value lang$add(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    Variant<Empty, int, String> accumulator { Empty {} };
    auto append = [&](auto arg) {
        Variant<Empty, int, String> value { Empty {} };
        if constexpr (requires { arg.template has<int>(); }) {
            if (arg.template has<Empty>() || arg.template has<int>() || arg.template has<String>())
                value = arg.template downcast<Empty, int, String>();
        } else {
            value = arg;
        }

        if (accumulator.template has<Empty>()) {
            accumulator = value;
        } else if (accumulator.template has<String>()) {
            if (!value.template has<Empty>()) {
                StringBuilder builder;
                builder.append(accumulator.get<String>());
                value.visit([&](auto& x) { builder.appendff("{}", x); }, [](Empty) {});
                accumulator = builder.build();
            }
        } else if (accumulator.template has<int>()) {
            if (value.template has<int>()) {
                accumulator = accumulator.template get<int>() + value.template get<int>();
            } else if (value.template has<String>()) {
                StringBuilder builder;
                value.visit([&](auto& x) { builder.appendff("{}", x); }, [](Empty) {});
                builder.append(value.get<String>());
                accumulator = builder.build();
            }
        }
    };
    for (auto& arg : args) {
        arg.value.visit(
            [&](Empty) { append(String("<empty>"sv)); },
            [&](NonnullRefPtr<FunctionNode> const&) { append(String("<function>"sv)); },
            [&](Type*) { append(String("<type>"sv)); },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) {
                for (auto& entry : crs->values)
                    append(entry.value);
            },
            [&](NativeFunctionType const&) { append(String("<fn>"sv)); },
            [&](auto const& value) { append(value); });
    }
    return { move(accumulator).downcast<Empty, int, String, Type*, NonnullRefPtr<FunctionNode>, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType>() };
}

void initialize_base(Context& context)
{
    context.scope.empend();
    context.comment_scope.empend();

    auto& scope = context.scope.last();

    scope.set("print", { NativeFunctionType { lang$print, { "print", "native" } } });
    scope.set("add", { NativeFunctionType { lang$add, { "native arithmetic addition operation" } } });
    scope.set("sub", { NativeFunctionType { lang$sub, { "native arithmetic subtract operation" } } });
}

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

    auto lexer = Lexer {};
#if 0
    for (;;) {
        auto maybe_token = lexer.next();
        if (maybe_token.is_error()) {
            warnln("Lex error: {} at {}:{}", maybe_token.error().error, maybe_token.error().where.line, maybe_token.error().where.column);
            return 1;
        }
        if (maybe_token.value().type == Token::Type::Eof)
            break;

        outln("- {}@{}:{} '{}'", to_underlying(maybe_token.value().type), maybe_token.value().source_range.start.line, maybe_token.value().source_range.start.column, maybe_token.value().text);
    }
#else
    auto parser = Parser { lexer };
    auto nodes = parser.parse_toplevel();
    if (nodes.is_error()) {
        warnln("Parse error: {} at {}:{}", nodes.error().error, nodes.error().where.line, nodes.error().where.column);
        return 1;
    }

#    if 0
    for (auto& node : nodes.value())
        node->dump(0);
#    endif

    Context context;
    initialize_base(context);
    for (auto& node : nodes.value())
        node->run(context);
#endif

    return 0;
}
