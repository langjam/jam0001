#include "AK/NonnullRefPtr.h"
#include "AST.h"
#include "Block.h"
#include "Primitives.h"

bool Primitives::has_primitive(String const& primitive)
{
#define __ENUMERATE_PRIMITIVE(P, S) \
    if (primitive == S)             \
        return true;
    ENUMERATE_PRIMITIVES()
#undef __ENUMERATE_PRIMITIVE

    return false;
}

Optional<Value> Primitives::call_primitive(NonnullRefPtr<Scope> scope, String const& name, Value target, Vector<Value> arguments)
{
#define __ENUMERATE_PRIMITIVE(P, S)               \
    if (name == S) {                              \
        auto value = P(scope, target, arguments); \
        if (scope->has_exception())               \
            return {};                            \
        return value;                             \
    }
    ENUMERATE_PRIMITIVES()
#undef __ENUMERATE_PRIMITIVE

    scope->set_exception(String::formatted("Undefined primitive {}", name));
    return {};
}

Optional<Value> Primitives::Print(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    if (arguments.size() != 0) {
        scope->set_exception("Expected no arguments for _Print");
        return {};
    }

    if (!target.has<String>()) {
        scope->set_exception("Expected comment as _Print argument");
        return {};
    }

    out("{}", target.get<String>());
    return target;
}

Optional<Value> Primitives::Add(NonnullRefPtr<Scope> scope, Value augend, Vector<Value> arguments)
{
    if (arguments.size() != 1) {
        scope->set_exception("Expected 1 argument for _Add:");
        return {};
    }

    Value& addend = arguments[0];
    if (!augend.has<String>()) {
        scope->set_exception("Expected comment as _Add: target");
        return {};
    }

    if (!addend.has<String>()) {
        scope->set_exception("Expected comment as _Add: argument");
        return {};
    }

    auto maybe_first = augend.get<String>().to_int();
    auto maybe_second = addend.get<String>().to_int();

    if (!maybe_first.has_value()) {
        scope->set_exception("_Add: target comment doesn't contain number");
        return {};
    }

    if (!maybe_second.has_value()) {
        scope->set_exception("_Add: argument comment doesn't contain number");
        return {};
    }

    return String::number(maybe_first.value() + maybe_second.value());
}

Optional<Value> Primitives::Negate(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    if (arguments.size() != 0) {
        scope->set_exception("Expected no arguments for _Negate");
        return {};
    }

    if (!target.has<String>()) {
        scope->set_exception("Expected comment as _Negate target");
        return {};
    }

    auto maybe_number = target.get<String>().to_int();

    if (!maybe_number.has_value()) {
        scope->set_exception("_Negate target comment doesn't contain number");
        return {};
    }

    return String::number(-maybe_number.value());
}

Optional<Value> Primitives::Eq(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    if (arguments.size() != 1) {
        scope->set_exception("Expected 1 argument for _Eq:");
        return {};
    }

    auto& other = arguments[0];

    if (!target.has<String>()) {
        scope->set_exception("Cannot compare blocks (yet!)");
        return {};
    }
    if (!other.has<String>()) {
        scope->set_exception("Cannot compare blocks (yet!)");
        return {};
    }

    return String { target.get<String>() == other.get<String>() ? "YEP" : "" };
}

Optional<Value> Primitives::DispatchIfTruthyFalsy(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    if (arguments.size() != 2) {
        scope->set_exception("Expected 2 arguments for _DispatchIfTruthy:Falsy:");
        return {};
    }

    auto& true_block = arguments[0];
    auto& false_block = arguments[1];

    if (!target.has<String>()) {
        scope->set_exception("_DispatchIfTruthy:Falsy: expects a comment as the target");
    }
    if (!true_block.has<NonnullRefPtr<Block>>()) {
        scope->set_exception("_DispatchIfTruthy:Falsy: requires truthy argument to be a block");
        return {};
    }
    if (!false_block.has<NonnullRefPtr<Block>>()) {
        scope->set_exception("_DispatchIfTruthy:Falsy: requires falsy argument to be a block");
        return {};
    }

    if (target.get<String>() == ""sv) {
        auto result = false_block.get<NonnullRefPtr<Block>>()->execute(scope, {});
        if (scope->has_exception())
            return {};
        return result;
    } else {
        auto result = true_block.get<NonnullRefPtr<Block>>()->execute(scope, {});
        if (scope->has_exception())
            return {};
        return result;
    }
}

Optional<Value> Primitives::Lt(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    if (arguments.size() != 1) {
        scope->set_exception("Expected 1 argument for _Lt:");
        return {};
    }

    auto& other = arguments[0];

    if (!target.has<String>()) {
        scope->set_exception("Cannot compare blocks (yet!)");
        return {};
    }
    if (!other.has<String>()) {
        scope->set_exception("Cannot compare blocks (yet!)");
        return {};
    }

    auto maybe_target_number = target.get<String>().to_int();
    auto maybe_other_number = other.get<String>().to_int();

    if (!maybe_target_number.has_value()) {
        scope->set_exception("_Lt: target comment doesn't contain number");
        return {};
    }

    if (!maybe_other_number.has_value()) {
        scope->set_exception("_Lt: other comment doesn't contain number");
        return {};
    }

    return String { maybe_target_number.value() < maybe_other_number.value() ? "YEP" : "" };
}

Optional<Value> Primitives::CallWith(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    return call_block(scope, target, 1, arguments);
}

Optional<Value> Primitives::CallWithWith(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    return call_block(scope, target, 2, arguments);
}

Optional<Value> Primitives::CallWithWithWith(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    return call_block(scope, target, 3, arguments);
}

Optional<Value> Primitives::CallWithWithWithWith(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    return call_block(scope, target, 4, arguments);
}

Optional<Value> Primitives::CallWithWithWithWithWith(NonnullRefPtr<Scope> scope, Value target, Vector<Value> arguments)
{
    return call_block(scope, target, 5, arguments);
}

Optional<Value> Primitives::call_block(NonnullRefPtr<Scope> scope, Value target, size_t count, Vector<Value> arguments)
{
    if (arguments.size() != count) {
        scope->set_exception(String::formatted("Expected {} arguments during block call, got {}", count, arguments.size()));
        return {};
    }

    if (!target.has<NonnullRefPtr<Block>>()) {
        scope->set_exception("Target of block call must be block");
        return {};
    }

    return target.get<NonnullRefPtr<Block>>()->execute(scope, arguments);
}
