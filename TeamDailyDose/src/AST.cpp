#include "AST.h"
#include "Primitives.h"
#include "Scope.h"

void StatementListNode::dump(int indent)
{
    print_indent(indent);
    outln("StatementListNode");

    print_indent(indent + 2);
    outln("statements:");

    for (auto& statement : m_statements) {
        statement.dump(indent + 4);
    }
}

void StatementListNode::print(int indent)
{
    for (auto& statement : m_statements) {
        print_indent(indent);
        statement.print(indent);
        outln(".");
    }
}

void MessageNode::print(int indent)
{
    m_target->print(indent);

    if (m_arguments.is_empty()) {
        out(" {}", m_message);
        return;
    }

    size_t offset = 0;
    for (auto& argument : m_arguments) {
        auto maybe_colon_offset = m_message.find(':', offset);
        VERIFY(maybe_colon_offset.has_value());
        auto colon_offset = maybe_colon_offset.value();

        out(" {}: ", m_message.substring_view(offset, colon_offset - offset));
        argument.print(indent);

        offset = colon_offset + 1;
    }
}

void FunctionDefinitionNode::print(int indent)
{
    if (m_arguments.size()) {
        size_t offset = 0;
        for (auto& argument : m_arguments) {
            auto maybe_colon_offset = m_name.find(':', offset);
            VERIFY(maybe_colon_offset.has_value());
            auto colon_offset = maybe_colon_offset.value();

            out("{}: {} ", m_name.substring_view(offset, colon_offset - offset), argument);

            offset = colon_offset + 1;
        }
    } else {
        out("{} ", m_name);
    }

    outln("(");
    m_statements->print(indent + 2);
    out(")");
}

void VariableAssignmentNode::print(int indent)
{
    out("{}: ", m_variable);
    m_value->print(indent);
}

void BlockNode::print(int indent)
{
    out("[");
    if (!m_arguments.is_empty()) {
        out("| ");
        for (auto& argument : m_arguments) {
            out("{}. ", argument);
        }
        out("|");
    }
    outln();

    m_statements->print(indent + 2);

    print_indent(indent);
    out("]");
}

Optional<Value> StatementListNode::evaluate(NonnullRefPtr<Scope> scope)
{
    Optional<Value> last_result;

    for (auto& statement : m_statements) {
        last_result = statement.evaluate(scope);
        if (scope->has_exception())
            return {};
    }

    return last_result;
}

Optional<Value> IdentifierNode::evaluate(NonnullRefPtr<Scope> scope)
{
    auto value = scope->lookup_variable(m_ident);
    if (value != nullptr) {
        return *value;
    }

    scope->set_exception(String::formatted("Undefined identifier {}", m_ident));
    return {};
}

Optional<Value> MessageNode::evaluate(NonnullRefPtr<Scope> scope)
{
    Optional<Value> maybe_target = m_target->evaluate(scope);
    if (scope->has_exception())
        return {};
    if (!maybe_target.has_value()) {
        scope->set_exception("Expression did not return value");
        return {};
    }

    Vector<Value> arguments;
    for (auto& argument : m_arguments) {
        auto maybe_argument = argument.evaluate(scope);
        if (scope->has_exception())
            return {};
        if (!maybe_argument.has_value()) {
            scope->set_exception("Argument did not return value");
            return {};
        }

        arguments.append(maybe_argument.value());
    }

    if (m_message[0] == '_') {
        auto result = Primitives::call_primitive(scope, m_message, maybe_target.value(), arguments);
        if (scope->has_exception())
            return {};
        return result;
    }

    auto maybe_function = scope->lookup_function(m_message);
    if (!maybe_function.is_null()) {
        auto result = static_cast<FunctionDefinitionNode*>(maybe_function.ptr())->call(scope, maybe_target.value(), arguments);
        if (scope->has_exception())
            return {};
        return result;
    }

    scope->set_exception(String::formatted("Undefined message {}", m_message));
    return {};
}

Optional<Value> FunctionDefinitionNode::evaluate(NonnullRefPtr<Scope> scope)
{
    scope->set_function(m_name, *this);
    return {};
}

Optional<Value> BlockNode::evaluate(NonnullRefPtr<Scope> scope)
{
    return Block::construct(scope, m_arguments, m_statements);
}

Optional<Value> FunctionDefinitionNode::call(NonnullRefPtr<Scope> parent_scope, Value target, Vector<Value> arguments)
{
    auto scope = Scope::construct(ChildScope, parent_scope);
    scope->set_variable("me", target);

    if (arguments.size() != m_arguments.size()) {
        parent_scope->set_exception(String::formatted("Expected {} arguments, got {}", m_arguments.size(), arguments.size()));
        return {};
    }

    for (size_t i = 0; i < arguments.size(); i++) {
        scope->set_variable(m_arguments[i], arguments[i]);
    }

    auto result = m_statements->evaluate(scope);
    if (scope->has_exception())
        return {};

    return result;
}

Optional<Value> VariableAssignmentNode::evaluate(NonnullRefPtr<Scope>)
{
    TODO();
}
