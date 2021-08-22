#pragma once

#include "types.h"
#include <AK/Demangle.h>

class ASTNode : public RefCounted<ASTNode> {
public:
    virtual ~ASTNode() = default;

    Value run(Context&);
    virtual void dump(int indent = 0)
    {
        warnln("{: >{}} {}", "", indent, demangle(typeid(*this).name()));
    }

private:
    friend class Statement;
    virtual Value execute(Context&) = 0;
};

class SyntheticNode : public ASTNode {
public:
    explicit SyntheticNode(Value value)
        : m_value(move(value))
    {
    }

    auto& value() const { return m_value; }

private:
    virtual Value execute(Context&) { return m_value; }
    virtual void dump(int indent) override { ASTNode::dump(indent); }

    Value m_value;
};

class Statement : public ASTNode {
public:
    explicit Statement(RefPtr<ASTNode> node)
        : m_node(move(node))
    {
    }

    auto& node() const { return m_node; }

private:
    virtual Value execute(Context& context) { return m_node->execute(context); }
    virtual void dump(int indent) override { m_node->dump(indent); }

    RefPtr<ASTNode> m_node;
};

class IntegerLiteral : public ASTNode {
public:
    explicit IntegerLiteral(int value)
        : m_value(value)
    {
    }

private:
    virtual Value execute(Context&) { return { m_value }; }
    virtual void dump(int indent) override;

    int m_value { 0 };
};

class StringLiteral : public ASTNode {
public:
    explicit StringLiteral(String value)
        : m_value(move(value))
    {
    }

private:
    virtual Value execute(Context&) { return { m_value }; }
    virtual void dump(int indent) override;

    String m_value;
};

class DirectMention : public ASTNode {
public:
    explicit DirectMention(Vector<String> keywords)
        : m_keywords(move(keywords))
    {
    }

private:
    virtual Value execute(Context&) override;
    virtual void dump(int indent) override;

    Vector<String> m_keywords;
};

class IndirectMention : public ASTNode {
public:
    explicit IndirectMention(ASTNode& node)
        : m_node(node)
    {
    }

private:
    virtual Value execute(Context&) override;
    virtual void dump(int indent) override;

    RefPtr<ASTNode> m_node;
};

class Variable;

class FunctionNode : public ASTNode {
public:
    explicit FunctionNode(Vector<NonnullRefPtr<Variable>> parameters, RefPtr<Variable> return_, Vector<NonnullRefPtr<ASTNode>> expressions)
        : m_parameters(move(parameters))
        , m_return(move(return_))
        , m_expressions(move(expressions))
    {
    }

    auto& parameters() const { return m_parameters; }
    auto& return_() const { return m_return; }
    auto& body() { return m_expressions; }

private:
    virtual Value execute(Context&) override;
    virtual void dump(int indent) override;

    Vector<NonnullRefPtr<Variable>> m_parameters;
    RefPtr<Variable> m_return;
    Vector<NonnullRefPtr<ASTNode>> m_expressions;
};

class Call : public ASTNode {
public:
    explicit Call(NonnullRefPtr<ASTNode> callee, Vector<NonnullRefPtr<ASTNode>> arguments)
        : m_callee(move(callee))
        , m_arguments(move(arguments))
    {
    }

private:
    Value execute(Context&) override;
    virtual void dump(int indent) override;

    NonnullRefPtr<ASTNode> m_callee;
    Vector<NonnullRefPtr<ASTNode>> m_arguments;
};

class Variable : public ASTNode {
public:
    explicit Variable(String name, RefPtr<ASTNode> type)
        : m_name(move(name))
        , m_type(move(type))
    {
    }

    auto& name() const { return m_name; }
    auto& type() const { return m_type; }

private:
    Value execute(Context&) override;
    virtual void dump(int indent) override;

    String m_name;
    RefPtr<ASTNode> m_type;
};

class RecordDecl : public ASTNode {
public:
    explicit RecordDecl(Vector<NonnullRefPtr<Variable>> decls)
        : m_decls(move(decls))
    {
    }

private:
    Value execute(Context&) override;
    virtual void dump(int indent) override;

    Vector<NonnullRefPtr<Variable>> m_decls;
};

class Comment : public ASTNode {
public:
    explicit Comment(String text)
        : m_text(move(text))
    {
    }

    auto& text() const { return m_text; }

private:
    Value execute(Context&) override;
    virtual void dump(int indent) override;

    String m_text;
};

class MemberAccess : public ASTNode {
public:
    explicit MemberAccess(String property, NonnullRefPtr<ASTNode> base)
        : m_property(move(property))
        , m_base(move(base))
    {
    }

private:
    Value execute(Context&) override;
    virtual void dump(int indent) override;

    String m_property;
    NonnullRefPtr<ASTNode> m_base;
};

class Assignment : public ASTNode {
public:
    explicit Assignment(NonnullRefPtr<Variable> var, NonnullRefPtr<ASTNode> value)
        : m_variable(move(var))
        , m_value(move(value))
    {
    }

private:
    virtual Value execute(Context&) override;
    virtual void dump(int indent) override;

    NonnullRefPtr<Variable> m_variable;
    NonnullRefPtr<ASTNode> m_value;
};
