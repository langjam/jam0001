#pragma once

#include "Utils.h"
#include "Value.h"
#include <AK/HashMap.h>
#include <AK/Optional.h>
#include <AK/RefCounted.h>
#include <AK/String.h>

class FunctionDefinitionNode;

enum ChildScopeTag {
    ChildScope
};

class Scope : public RefCounted<Scope> {
    CONSTRUCT(Scope)
public:
    ~Scope()
    {
        if (!m_parent_scope.is_null() && m_exception.has_value()) {
            m_parent_scope->set_exception(m_exception.release_value());
        }
    }

    Value const* lookup_variable(String const& name) const;
    void set_variable(String const& name, Value value);

    RefPtr<FunctionDefinitionNode> lookup_function(String const& name) const;
    void set_function(String const& name, NonnullRefPtr<FunctionDefinitionNode> function);

    bool has_exception() const { return m_exception.has_value(); }
    String const& exception() const
    {
        VERIFY(has_exception());
        return m_exception.value();
    }
    void set_exception(String exception) { m_exception = move(exception); }
    void clear_exception() { m_exception = {}; }

private:
    Scope(ChildScopeTag, RefPtr<Scope> parent_scope)
        : m_parent_scope(parent_scope)
    {
    }

    Scope()
    {
    }

    HashMap<String, Value> m_variables;
    HashMap<String, NonnullRefPtr<FunctionDefinitionNode>> m_functions;
    RefPtr<Scope> m_parent_scope;
    Optional<String> m_exception;
};
