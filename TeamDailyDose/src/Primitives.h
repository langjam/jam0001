#pragma once

#include "Scope.h"
#include <AK/NonnullRefPtr.h>

#define ENUMERATE_PRIMITIVES()                                                 \
    __ENUMERATE_PRIMITIVE(Print, "_Print"sv)                                   \
    __ENUMERATE_PRIMITIVE(Add, "_Add:"sv)                                      \
    __ENUMERATE_PRIMITIVE(Negate, "_Negate"sv)                                 \
    __ENUMERATE_PRIMITIVE(Eq, "_Eq:"sv)                                        \
    __ENUMERATE_PRIMITIVE(DispatchIfTruthyFalsy, "_DispatchIfTruthy:Falsy:"sv) \
    __ENUMERATE_PRIMITIVE(Lt, "_Lt:"sv)                                        \
    __ENUMERATE_PRIMITIVE(CallWith, "_CallWith:"sv)                            \
    __ENUMERATE_PRIMITIVE(CallWithWith, "_CallWith:With:"sv)                   \
    __ENUMERATE_PRIMITIVE(CallWithWithWith, "_CallWith:With:With:"sv)          \
    __ENUMERATE_PRIMITIVE(CallWithWithWithWith, "_CallWith:With:With:With:"sv) \
    __ENUMERATE_PRIMITIVE(CallWithWithWithWithWith, "_CallWith:With:With:With:With:"sv)

class Primitives {
public:
    static bool has_primitive(String const& primitive);
    static Optional<Value> call_primitive(NonnullRefPtr<Scope>, String const& name, Value target, Vector<Value> arguments);
    static Optional<Value> call_block(NonnullRefPtr<Scope>, Value target, size_t count, Vector<Value> arguments);

#define __ENUMERATE_PRIMITIVE(P, S) \
    static Optional<Value> P(NonnullRefPtr<Scope>, Value target, Vector<Value> arguments);
    ENUMERATE_PRIMITIVES()
#undef __ENUMERATE_PRIMITIVE
};
