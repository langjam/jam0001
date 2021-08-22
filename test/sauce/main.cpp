#include "parser.h"
#include <AK/Format.h>
#include <AK/Function.h>
#include <AK/Random.h>
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

template<typename Operator>
static void fold_append(auto& accumulator, auto&& arg)
{
    Variant<Empty, int, String> value { Empty {} };
    if constexpr (requires { arg.template has<int>(); }) {
        if (arg.template has<Empty>() || arg.template has<int>())
            value = arg.template downcast<Empty, int, String>();
        else if (arg.template has<NonnullRefPtr<CommentResolutionSet>>()) {
            for (auto& entry : arg.template get<NonnullRefPtr<CommentResolutionSet>>()->values)
                fold_append<Operator>(accumulator, entry.value);
            return;
        }
    } else if constexpr (IsSame<RemoveCVReference<decltype(arg)>, int> || IsSame<RemoveCVReference<decltype(arg)>, String>) {
        value = arg;
    }

    if (accumulator.template has<Empty>()) {
        accumulator = value;
    } else if (accumulator.template has<int>()) {
        if (value.template has<int>()) {
            accumulator = Operator {}(accumulator.template get<int>(), value.template get<int>());
        }
    } else if (accumulator.template has<String>()) {
        if (value.template has<String>()) {
            accumulator = Operator {}(accumulator.template get<String>(), value.template get<String>());
        }
    }
};

template<typename Operator>
Value lang$fold_op(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    Variant<Empty, int, String> accumulator { Empty {} };
    for (auto& arg : args) {
        arg.value.visit(
            [&](Empty) { fold_append<Operator>(accumulator, Empty {}); },
            [&](FunctionValue const&) { fold_append<Operator>(accumulator, String("<function>"sv)); },
            [&](NonnullRefPtr<Type> const&) { fold_append<Operator>(accumulator, String("<type>"sv)); },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) {
                for (auto& entry : crs->values)
                    fold_append<Operator>(accumulator, entry.value);
            },
            [&](NativeFunctionType const&) { fold_append<Operator>(accumulator, String("<fn>"sv)); },
            [&](RecordValue const& rv) { fold_append<Operator>(accumulator, String("<record>")); },
            [&](auto const& value) { fold_append<Operator>(accumulator, value); });
    }
    return { move(accumulator).downcast<Empty, int, String, NonnullRefPtr<Type>, FunctionValue, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType, RecordValue>() };
}

static void add_append(auto& accumulator, auto&& arg)
{
    Variant<Empty, int, String> value { Empty {} };
    if constexpr (requires { arg.template has<int>(); }) {
        if (arg.template has<Empty>() || arg.template has<int>() || arg.template has<String>())
            value = arg.template downcast<Empty, int, String>();
        else if (arg.template has<NonnullRefPtr<CommentResolutionSet>>()) {
            for (auto& entry : arg.template get<NonnullRefPtr<CommentResolutionSet>>()->values)
                add_append(accumulator, entry.value);
            return;
        }
    } else {
        value = arg;
    }

    if (accumulator.template has<Empty>()) {
        accumulator = value;
    } else if (accumulator.template has<String>()) {
        if (!value.template has<Empty>()) {
            StringBuilder builder;
            builder.append(accumulator.template get<String>());
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

Value lang$add(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    Variant<Empty, int, String> accumulator { Empty {} };
    for (auto& arg : args) {
        arg.value.visit(
            [&](Empty) { add_append(accumulator, String("<empty>"sv)); },
            [&](FunctionValue const&) { add_append(accumulator, String("<function>"sv)); },
            [&](NonnullRefPtr<Type> const&) { add_append(accumulator, String("<type>"sv)); },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) {
                for (auto& entry : crs->values)
                    add_append(accumulator, entry.value);
            },
            [&](NativeFunctionType const&) { add_append(accumulator, String("<fn>"sv)); },
            [&](RecordValue const& rv) { add_append(accumulator, String("<record>"sv)); },
            [&](auto const& value) { add_append(accumulator, value); });
    }
    return { move(accumulator).downcast<Empty, int, String, NonnullRefPtr<Type>, FunctionValue, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType, RecordValue>() };
}

static bool truth(Value const& condition)
{
    return condition.value.visit(
        [](Empty) -> bool { return false; },
        [](FunctionValue const&) -> bool { return true; },
        [](NonnullRefPtr<Type> const&) -> bool { return true; },
        [](NonnullRefPtr<CommentResolutionSet> const& crs) -> bool {
            return all_of(crs->values, truth);
        },
        [](NativeFunctionType const&) -> bool { return true; },
        [](RecordValue const&) { return true; },
        [](auto const& value) -> bool {
            if constexpr (requires { (bool)value; })
                return (bool)value;
            else if constexpr (requires { value.is_empty(); })
                return !value.is_empty();
            else
                return true;
        });
}

Value& flatten(Value& input)
{
    if (auto ptr = input.value.get_pointer<NonnullRefPtr<CommentResolutionSet>>()) {
        if ((*ptr)->values.size() == 1)
            return flatten((*ptr)->values.first());
    }

    return input;
}

Value lang$cond(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    size_t i = 0;
    for (; i + 1 < count; i += 2) {
        auto& condition = args[i];
        auto& value = args[i + 1];
        if (truth(condition))
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

Value lang$loop(Context& context, void* ptr, size_t count)
{
    // loop(start, step_fn, stop_cond) :: v=start; while(!stop_cond(v)) v = step_cond(v); return v;
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    if (args.size() < 3)
        return { Empty {} };

    auto value = args[0];
    auto& step = args[1];
    auto& stop = args[2];

    auto step_fn = [&] {
        value = create<Call>(
            static_ptr_cast<ASTNode>(create<SyntheticNode>(step)),
            Vector { static_ptr_cast<ASTNode>(create<SyntheticNode>(value)) })
                    ->run(context);
    };

    auto stop_fn = [&] {
        auto res = create<Call>(
            static_ptr_cast<ASTNode>(create<SyntheticNode>(stop)),
            Vector { static_ptr_cast<ASTNode>(create<SyntheticNode>(value)) })
                       ->run(context);
        return truth(res);
    };

    while (!stop_fn())
        step_fn();

    return value;
}

Value lang$nth(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    if (args.size() < 2)
        return { Empty {} };

    auto index = flatten(args[0]).value.get_pointer<int>();
    auto subject = flatten(args[1]).value.get_pointer<String>();

    if (!index || !subject)
        return { Empty {} };

    return { String::repeated(subject->operator[](*index), 1) };
}

Value lang$slice(Context&, void* ptr, size_t count)
{
    Span<Value> args { reinterpret_cast<Value*>(ptr), count };
    if (args.size() < 3)
        return { Empty {} };

    auto index = flatten(args[0]).value.get_pointer<int>();
    auto size = flatten(args[1]).value.get_pointer<int>();
    auto subject = flatten(args[2]).value.get_pointer<String>();

    if (!index || !size || !subject)
        return { Empty {} };

    return { subject->substring(*index, *size) };
}

struct Sub {
    int operator()(int a, int b) { return a - b; }
    int operator()(String const&, String const&) { return 0; }
};

struct Mul {
    int operator()(int a, int b) { return a * b; }
    int operator()(String const&, String const&) { return 0; }
};

struct Div {
    int operator()(int a, int b) { return a / b; }
    int operator()(String const&, String const&) { return 0; }
};

struct Mod {
    int operator()(int a, int b) { return a % b; }
    int operator()(String const&, String const&) { return 0; }
};

struct Greater {
    int operator()(int a, int b) { return a > b; }
    int operator()(String const& a, String const& b) { return a > b; }
};

struct Equal {
    int operator()(int a, int b) { return a == b; }
    int operator()(String const& a, String const& b) { return a == b; }
};

struct Flat {
    template<typename T>
    T operator()(T a, T b)
    {
        if (get_random<bool>())
            return a;
        return b;
    }
};

void initialize_base(Context& context)
{
    context.scope.empend();
    context.comment_scope.empend();
    context.last_call_scope_start = 0;

    auto& scope = context.scope.last();

    scope.set("print", { NativeFunctionType { lang$print, { "print function", "native operation" } } });
    scope.set("add", { NativeFunctionType { lang$add, { "native arithmetic addition operation" } } });
    scope.set("sub", { NativeFunctionType { lang$fold_op<Sub>, { "native arithmetic subtract operation" } } });
    scope.set("mul", { NativeFunctionType { lang$fold_op<Mul>, { "native arithmetic multiply operation" } } });
    scope.set("div", { NativeFunctionType { lang$fold_op<Div>, { "native arithmetic divide operation" } } });
    scope.set("mod", { NativeFunctionType { lang$fold_op<Mod>, { "native arithmetic modulus operation" } } });
    scope.set("cond", { NativeFunctionType { lang$cond, { "native conditional selection operation" } } });
    scope.set("is", { NativeFunctionType { lang$is, { "native comment query operation" } } });
    scope.set("loop", { NativeFunctionType { lang$loop, { "native loop flow operation" } } });
    scope.set("gt", { NativeFunctionType { lang$fold_op<Greater>, { "native comparison greater_than operation" } } });
    scope.set("eq", { NativeFunctionType { lang$fold_op<Equal>, { "native comparison equality operation" } } });
    scope.set("collapse", { NativeFunctionType { lang$fold_op<Flat>, { "native probability collapse flatten operation" } } });
    scope.set("nth", { NativeFunctionType { lang$nth, { "native string indexing operation" } } });
    scope.set("slice", { NativeFunctionType { lang$slice, { "native string slicing operation" } } });

    // types
    scope.set("int", { create<Type>(NativeType::Int) });
    scope.set("string", { create<Type>(NativeType::String) });
    scope.set("any", { create<Type>(NativeType::Any) });
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
