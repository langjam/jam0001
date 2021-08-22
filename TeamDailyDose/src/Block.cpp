#include "AST.h"
#include "Block.h"

Block::Block(NonnullRefPtr<Scope> scope, Vector<String> arguments, NonnullRefPtr<StatementListNode> statements)
    : m_scope(Scope::construct(ChildScope, move(scope)))
    , m_arguments(move(arguments))
    , m_statements(move(statements))
{
}

Optional<Value> Block::execute(NonnullRefPtr<Scope> calling_scope, Vector<Value> arguments)
{
    if (arguments.size() != m_arguments.size()) {
        calling_scope->set_exception(String::formatted("Expected {} arguments, got {}", m_arguments.size(), arguments.size()));
        return {};
    }

    if (auto* me = m_scope->lookup_variable("me"); me != nullptr) {
        m_scope->set_variable("me", *me);
    } else {
        m_scope->set_variable("me", String::empty());
    }

    for (size_t i = 0; i < m_arguments.size(); i++) {
        m_scope->set_variable(m_arguments[i], arguments[i]);
    }

    auto result = m_statements->evaluate(m_scope);
    if (m_scope->has_exception()) {
        calling_scope->set_exception(m_scope->exception());
        m_scope->clear_exception();
        return {};
    }

    return result;
}
