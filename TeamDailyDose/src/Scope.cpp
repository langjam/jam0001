#include "AST.h"
#include "Scope.h"

Value const* Scope::lookup_variable(String const& name) const
{
    auto it = m_variables.find(name);
    if (it != m_variables.end()) {
        return &it->value;
    }

    if (!m_parent_scope.is_null()) {
        auto variable = m_parent_scope->lookup_variable(name);
        if (variable != nullptr) {
            return variable;
        }
    }

    return nullptr;
}

void Scope::set_variable(String const& name, Value value)
{
    m_variables.set(name, move(value));
}

RefPtr<FunctionDefinitionNode> Scope::lookup_function(String const& name) const
{
    auto it = m_functions.find(name);
    if (it != m_functions.end()) {
        return it->value;
    }

    if (!m_parent_scope.is_null()) {
        auto function = m_parent_scope->lookup_function(name);
        if (!function.is_null()) {
            return function;
        }
    }

    return {};
}

void Scope::set_function(String const& name, NonnullRefPtr<FunctionDefinitionNode> function)
{
    if (!m_parent_scope.is_null()) {
        m_parent_scope->set_function(name, function);
        return;
    }

    m_functions.set(name, move(function));
}
