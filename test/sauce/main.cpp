#include "parser.h"
#include <AK/Format.h>
#include <AK/Function.h>
#include <AK/StringView.h>
#include <AK/TypeCasts.h>
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
            [](FunctionValue const&) { out("<fn ref>"); },         // FIXME
            [](NonnullRefPtr<Type> const&) { out("<type ref>"); }, // FIXME
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
            [&](RecordValue const& rv) {
                out("(");
                auto first = true;
                for (auto& entry : rv.members) {
                    if (!first)
                        out(" ");
                    first = false;
                    print_value(entry);
                }
                out(")");
            },
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
    auto append = [&](auto&& arg) {
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
            [&](FunctionValue const&) { append(String("<function>"sv)); },
            [&](NonnullRefPtr<Type> const&) { append(String("<type>"sv)); },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) {
                for (auto& entry : crs->values)
                    append(entry.value);
            },
            [&](NativeFunctionType const&) { append(String("<fn>"sv)); },
            [&](RecordValue const& rv) { append(String("<record>")); },
            [&](auto const& value) { append(value); });
    }
    return { move(accumulator).downcast<Empty, int, String, NonnullRefPtr<Type>, FunctionValue, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType, RecordValue>() };
}

Value lang$add(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    Variant<Empty, int, String> accumulator { Empty {} };
    auto append = [&](auto&& arg) {
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
            [&](FunctionValue const&) { append(String("<function>"sv)); },
            [&](NonnullRefPtr<Type> const&) { append(String("<type>"sv)); },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) {
                for (auto& entry : crs->values)
                    append(entry.value);
            },
            [&](NativeFunctionType const&) { append(String("<fn>"sv)); },
            [&](RecordValue const& rv) { append(String("<record>"sv)); },
            [&](auto const& value) { append(value); });
    }
    return { move(accumulator).downcast<Empty, int, String, NonnullRefPtr<Type>, FunctionValue, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType, RecordValue>() };
}

Value lang$cond(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    size_t i = 0;
    for (; i + 1 < count; i += 2) {
        auto& condition = args[i];
        auto& value = args[i + 1];
        if (condition.value.visit(
                [&](Empty) -> bool { return false; },
                [&](FunctionValue const&) -> bool { return true; },
                [&](NonnullRefPtr<Type> const&) -> bool { return true; },
                [&](NonnullRefPtr<CommentResolutionSet> const& crs) -> bool {
                    // TODO.
                    return true;
                },
                [&](NativeFunctionType const&) -> bool { return true; },
                [](RecordValue const&) { return true; },
                [&](auto const& value) -> bool {
                    if constexpr (requires { (bool)value; })
                        return (bool)value;
                    else if constexpr (requires { value.is_empty(); })
                        return !value.is_empty();
                    else
                        return true;
                }))
            return value;
    }
    if (i < count)
        return args[count - 1];

    return { Empty {} };
}

Value lang$is(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    if (args.size() < 2)
        return { Empty {} };

    auto& value = args[0];
    if (!value.value.has<FunctionValue>())
        return { Empty {} };

    auto& query = args[1];
    if (!query.value.has<String>())
        return { Empty {} };

    auto words = query.value.get<String>().split(' ');

    auto& fn = value.value.get<FunctionValue>();
    Vector<bool> found;
    found.resize(words.size());

    for (auto entry : fn.node->body()) {
        auto const* ptr = entry.ptr();
        if (is<Statement>(*entry))
            ptr = static_cast<Statement const*>(ptr)->node().ptr();

        if (!is<Comment>(*ptr))
            continue;

        auto comment = static_cast<Comment const*>(ptr);
        for (auto it = words.begin(); it != words.end(); ++it) {
            if (found[it.index()])
                continue;
            if (comment->text().contains(*it))
                found[it.index()] = true;
        }

        if (all_of(found, [](auto x) { return x; }))
            break;
    }

    if (all_of(found, [](auto x) { return x; }))
        return { 1 };

    return { 0 };
}

void initialize_base(Context& context)
{
    context.scope.empend();
    context.comment_scope.empend();

    auto& scope = context.scope.last();

    scope.set("print", { NativeFunctionType { lang$print, { "print", "native" } } });
    scope.set("add", { NativeFunctionType { lang$add, { "native arithmetic addition operation" } } });
    scope.set("sub", { NativeFunctionType { lang$sub, { "native arithmetic subtract operation" } } });
    scope.set("cond", { NativeFunctionType { lang$cond, { "native conditional selection operation" } } });
    scope.set("is", { NativeFunctionType { lang$is, { "native comment query operation" } } });
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
#    else
    Context context;
    initialize_base(context);
    for (auto& node : nodes.value())
        node->run(context);
#    endif
#endif

    return 0;
}
