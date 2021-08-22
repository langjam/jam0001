#pragma once

#include "AK/NonnullRefPtr.h"
#include "Block.h"
#include "Scope.h"
#include "Utils.h"
#include "Value.h"
#include <AK/NonnullRefPtrVector.h>
#include <AK/RefCounted.h>
#include <AK/StdLibExtras.h>
#include <AK/Variant.h>

class ASTNode : public RefCounted<ASTNode> {
public:
    virtual ~ASTNode()
    {
    }

    virtual void dump(int indent = 0) = 0;
    virtual void print(int indent = 0) = 0;
    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) = 0;

protected:
    void print_indent(int indent)
    {
        for (int i = 0; i < indent; i++)
            out(" ");
    }
};

class ParenthesizedExpressionNode final : public ASTNode {
    CONSTRUCT(ParenthesizedExpressionNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("ParenthesizedExpressionNode");

        print_indent(indent + 2);
        outln("expression:");

        m_expression->dump(indent + 4);
    }

    virtual void print(int indent = 0) override
    {
        out("(");
        m_expression->print(indent);
        out(")");
    }

    virtual Optional<Value> evaluate(NonnullRefPtr<Scope> scope) override
    {
        return m_expression->evaluate(scope);
    }

private:
    ParenthesizedExpressionNode(NonnullRefPtr<ASTNode> expression)
        : m_expression(move(expression))
    {
    }

    NonnullRefPtr<ASTNode> m_expression;
};

class CommentNode final : public ASTNode {
    CONSTRUCT(CommentNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("CommentNode \"{}\"", m_value);
    }

    virtual void print(int = 0) override
    {
        out("\"{}\"", m_value);
    }

    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override
    {
        return m_value;
    }

private:
    CommentNode(String value)
        : m_value(move(value))
    {
    }

    String m_value;
};

class IdentifierNode final : public ASTNode {
    CONSTRUCT(IdentifierNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("IdentifierNode \"{}\"", m_ident);
    }

    virtual void print(int = 0) override
    {
        out("{}", m_ident);
    }

    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override;

private:
    IdentifierNode(String ident)
        : m_ident(move(ident))
    {
    }

    String m_ident;
};

class MessageNode final : public ASTNode {
    CONSTRUCT(MessageNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("MessageNode");

        print_indent(indent + 2);
        outln("target:");
        m_target->dump(indent + 4);

        print_indent(indent + 2);
        outln("message: \"{}\"", m_message);

        print_indent(indent + 2);
        outln("arguments:");
        for (auto& arg : m_arguments) {
            arg.dump(indent + 4);
        }
    }

    virtual void print(int indent = 0) override;
    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override;

private:
    MessageNode(NonnullRefPtr<ASTNode> target, String message, NonnullRefPtrVector<ASTNode> arguments)
        : m_target(move(target))
        , m_message(move(message))
        , m_arguments(move(arguments))
    {
    }

    NonnullRefPtr<ASTNode> m_target;
    String m_message;
    NonnullRefPtrVector<ASTNode> m_arguments;
};

class StatementListNode final : public ASTNode {
    CONSTRUCT(StatementListNode)
public:
    virtual void dump(int indent = 0) override;
    virtual void print(int indent = 0) override;
    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override;

    NonnullRefPtrVector<ASTNode> statements() { return m_statements; }

private:
    StatementListNode(NonnullRefPtrVector<ASTNode> statements)
        : m_statements(move(statements))
    {
    }

    NonnullRefPtrVector<ASTNode> m_statements;
};

class FunctionDefinitionNode final : public ASTNode {
    CONSTRUCT(FunctionDefinitionNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("FunctionDefinitionNode");

        print_indent(indent + 2);
        outln("name: \"{}\"", m_name);

        print_indent(indent + 2);
        outln("arguments: {}", m_arguments);

        print_indent(indent + 2);
        outln("statements:");
        for (auto& statement : m_statements->statements()) {
            statement.dump(indent + 4);
        }
    }

    virtual void print(int indent = 0) override;
    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override;

    Optional<Value> call(NonnullRefPtr<Scope>, Value target, Vector<Value> arguments);

private:
    FunctionDefinitionNode(String name, Vector<String> arguments, NonnullRefPtr<StatementListNode> statements)
        : m_name(move(name))
        , m_arguments(move(arguments))
        , m_statements(move(statements))
    {
    }

    String m_name;
    Vector<String> m_arguments;
    NonnullRefPtr<StatementListNode> m_statements;
};

class VariableAssignmentNode final : public ASTNode {
    CONSTRUCT(VariableAssignmentNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("VariableAssignmentNode");

        print_indent(indent + 2);
        outln("name: \"{}\"", m_variable);

        print_indent(indent + 2);
        outln("value:");
        m_value->dump(indent + 4);
    }

    virtual void print(int indent = 0) override;
    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override;

private:
    VariableAssignmentNode(String variable, NonnullRefPtr<ASTNode> value)
        : m_variable(move(variable))
        , m_value(move(value))
    {
    }

    String m_variable;
    NonnullRefPtr<ASTNode> m_value;
};

class BlockNode final : public ASTNode {
    CONSTRUCT(BlockNode)
public:
    virtual void dump(int indent = 0) override
    {
        print_indent(indent);
        outln("BlockNode");

        print_indent(indent + 2);
        outln("arguments: {}", m_arguments);

        print_indent(indent + 2);
        outln("statements:");
        m_statements->dump(indent + 4);
    }

    virtual void print(int indent = 0) override;
    virtual Optional<Value> evaluate(NonnullRefPtr<Scope>) override;

private:
    BlockNode(Vector<String> arguments, NonnullRefPtr<StatementListNode> statements)
        : m_arguments(move(arguments))
        , m_statements(move(statements))
    {
    }

    Vector<String> m_arguments;
    NonnullRefPtr<StatementListNode> m_statements;
};
