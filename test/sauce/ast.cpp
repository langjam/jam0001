#include "ast.h"
#include <AK/Function.h>
#include <AK/TypeCasts.h>

Value ASTNode::run(Context& context)
{
    auto value = execute(context);
    if (is<Statement>(this) && !is<Comment>(*static_cast<Statement*>(this)->node())) {
        auto comments = move(context.unassigned_comments);
        for (auto& entry : comments) {
            auto all = context.comment_scope.last().get(entry).value_or({});
            all.append(value);
            context.comment_scope.last().set(entry, move(all));
        }
    }

    return value;
}

void IntegerLiteral::dump(int indent)
{
    ASTNode::dump(indent);
    warnln("{: >{}}(Value) {}", "", indent, m_value, indent + 1);
}

void StringLiteral::dump(int indent)
{
    ASTNode::dump(indent);
    warnln("{: >{}}(Value) {}", "", indent, m_value, indent + 1);
}

void DirectMention::dump(int indent)
{
    ASTNode::dump(indent);
    for (auto& entry : m_keywords)
        warnln("{: >{}}(Query) {}", "", indent, entry, indent + 1);
}

Value DirectMention::execute(Context& context)
{
    auto crs = create<CommentResolutionSet>();
    for (size_t i = context.scope.size(); i > 0; --i) {
        auto& scope = context.scope[i - 1];
        for (auto& entry : scope) {
            if (auto nfn = entry.value.value.get_pointer<NativeFunctionType>()) {
                for (auto& query : m_keywords) {
                    auto found = false;
                    for (auto& comment : nfn->comments) {
                        if (comment.contains(query)) {
                            found = true;
                            goto out;
                        }
                    }
                out:;
                    if (!found)
                        goto not_this_entry;
                }
                crs->values.append(entry.value);
                continue;
            }
        not_this_entry:;
        }
    }
    for (size_t i = context.comment_scope.size(); i > 0; --i) {
        auto& scope = context.comment_scope[i - 1];
        for (auto& entry : scope) {
            auto fail = false;
            for (auto& query : m_keywords) {
                if (!entry.key->text().contains(query)) {
                    fail = true;
                    break;
                }
            }
            if (!fail) {
                for (auto& value : entry.value)
                    crs->values.append(value);
            }
        }
    }
    return Value { move(crs) };
}

void IndirectMention::dump(int indent)
{
    ASTNode::dump(indent);
    m_node->dump(indent + 1);
}

void FunctionNode::dump(int indent)
{
    ASTNode::dump(indent);
    // FIXME
}

Value FunctionNode::execute(Context&)
{
    return { NonnullRefPtr<FunctionNode>(*this) };
}

void Call::dump(int indent)
{
    ASTNode::dump(indent);
    warnln("{: >{}}(Callee) ", "", indent + 1);
    m_callee->dump(indent + 2);
    warnln("{: >{}}(Arguments)", "", indent + 1);
    for (auto& arg : m_arguments)
        arg->dump(indent + 2);
}

Value Call::execute(Context& context)
{
    Vector<Value> arguments;
    for (auto& arg : m_arguments)
        arguments.append(arg->run(context));
    auto fn = m_callee->run(context);
    Function<Value(Value&)> execute = [&](auto& callee) -> Value {
        if (auto ptr = callee.value.template get_pointer<NativeFunctionType>())
            return ptr->fn(context, arguments.data(), arguments.size());
        if (auto ptr = callee.value.template get_pointer<NonnullRefPtr<CommentResolutionSet>>()) {
            auto set_ptr = ptr->ptr();
            auto crs = create<CommentResolutionSet>();
            for (auto& entry : set_ptr->values)
                crs->values.append(execute(entry));
            return Value { move(crs) };
        }
        if (auto ptr = callee.value.template get_pointer<NonnullRefPtr<FunctionNode>>()) {
            FunctionNode* node_ptr = ptr->ptr();
            context.scope.template empend();
            context.comment_scope.template empend();
            auto& scope = context.scope.last();
            size_t i = 0;
            for (auto& param : node_ptr->parameters()) {
                if (arguments.size() > i)
                    scope.set(param->name(), arguments[i]);
                else
                    scope.set(param->name(), { Empty {} });
                ++i;
            }

            if (node_ptr->return_())
                scope.set(node_ptr->return_()->name(), { Empty {} });

            auto old_comments = move(context.unassigned_comments);
            for (auto& node : node_ptr->body())
                node->run(context);

            Value result { Empty{} };
            if (node_ptr->return_())
                result = scope.get(node_ptr->return_()->name()).value();

            context.scope.take_last();
            context.comment_scope.take_last();
            context.unassigned_comments = move(old_comments);

            return result;
        }
        return { Empty {} };
    };

    return execute(fn);
}

void Variable::dump(int indent)
{
    ASTNode::dump(indent);
    warnln("{: >{}}(Name) {}", "", indent + 1, m_name);
    if (m_type) {
        warnln("{: >{}}(Type) ", "", indent + 1);
        m_type->dump(indent + 2);
    }
}

Value Variable::execute(Context& context)
{
    for (size_t i = context.scope.size(); i > 0; --i) {
        auto& scope = context.scope[i - 1];
        if (!scope.contains(m_name))
            continue;

        auto& value = scope.find(m_name)->value;
        if (m_type)
            TODO();

        return value;
    }
    return { Empty {} };
}

void RecordDecl::dump(int indent)
{
    ASTNode::dump(indent);
    // FIXME
}

void Comment::dump(int indent)
{
    ASTNode::dump(indent);
    warnln("{: >{}}(Text) '{}'", "", indent + 1, m_text);
}

Value Comment::execute(Context& context)
{
    context.unassigned_comments.append(this);
    return { Empty {} };
}

void MemberAccess::dump(int indent)
{
    ASTNode::dump(indent);
    // FIXME
}

void Assignment::dump(int indent)
{
    ASTNode::dump(indent);
    warnln("{: >{}}(Variable) ", "", indent + 1);
    static_ptr_cast<ASTNode>(m_variable)->dump(indent + 2);
    warnln("{: >{}}(Value) ", "", indent + 1);
    m_value->dump(indent + 2);
}

Value Assignment::execute(Context& context)
{
    auto value = m_value->run(context);
    context.scope.last().set(m_variable->name(), value);
    return value;
}
