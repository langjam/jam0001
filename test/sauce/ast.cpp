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
    if (m_keywords.is_empty())
        return Value { move(crs) };

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

Value IndirectMention::execute(Context& context)
{
    auto mention = m_node->run(context);
    if (!mention.value.has<String>())
        return { Empty {} };
    
    auto words = mention.value.get<String>().split(' ');
    return create<DirectMention>(move(words))->run(context);
}

void FunctionNode::dump(int indent)
{
    ASTNode::dump(indent);
    // FIXME
}

Value FunctionNode::execute(Context& context)
{
    auto scope = context.scope;
    scope.take_first();
    auto cscope = context.comment_scope;
    cscope.take_first();
    return {
        FunctionValue {
            NonnullRefPtr<FunctionNode>(*this),
            move(scope),
            move(cscope),
        },
    };
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
        if (auto ptr = callee.value.template get_pointer<FunctionValue>()) {
            auto last_scope = context.scope.size();
            auto last_cscope = context.comment_scope.size();

            context.scope.extend(ptr->scope);
            context.comment_scope.extend(ptr->comment_scope);

            context.scope.template empend();
            context.comment_scope.template empend();
            auto& scope = context.scope.last();

            size_t i = 0;
            for (auto& param : ptr->node->parameters()) {
                if (arguments.size() > i)
                    scope.set(param->name(), arguments[i]);
                else
                    scope.set(param->name(), { Empty {} });
                ++i;
            }

            if (ptr->node->return_())
                scope.set(ptr->node->return_()->name(), { Empty {} });

            auto old_comments = move(context.unassigned_comments);
            for (auto& node : ptr->node->body())
                node->run(context);

            Value result { Empty {} };
            if (ptr->node->return_())
                result = scope.get(ptr->node->return_()->name()).value();

            while (context.scope.size() > last_scope)
                context.scope.take_last();
            while (context.comment_scope.size() > last_cscope)
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

Value RecordDecl::execute(Context& context)
{
    Vector<TypeName> members;
    for (auto& entry : m_decls) {
        TypeName member {
            .name = entry->name(),
            .type = create<Type>(NativeType::Any),
        };
        if (entry->type()) {
            auto type = const_cast<RefPtr<ASTNode>&>(entry->type())->run(context);
            if (type.value.has<NonnullRefPtr<Type>>())
                member.type = type.value.get<NonnullRefPtr<Type>>();
        }
        members.append(move(member));
    }
    return { create<Type>(move(members)) };
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

Value MemberAccess::execute(Context& context)
{
    auto value = m_base->run(context);
    Function<Value(Value&)> visit = [&](auto& value) {
        return value.value.visit(
            [](Empty) -> Value { return { Empty {} }; },
            [&](String const& string) -> Value {
                if (m_property == "length"sv)
                    return { string.length() };
                return { Empty {} };
            },
            [&](int value) -> Value {
                if (m_property.is_one_of("negated"sv, "neg"sv))
                    return { -value };
                
                return { Empty {} };
            },
            [](FunctionValue const&) -> Value { return { Empty {} }; },
            [](NativeFunctionType const&) -> Value { return { Empty {} }; },
            [](NonnullRefPtr<Type> const&) -> Value { return { Empty {} }; },
            [&](NonnullRefPtr<CommentResolutionSet> const& crs) -> Value {
                auto res_crs = create<CommentResolutionSet>();
                for (auto& entry : crs->values)
                    res_crs->values.append(visit(const_cast<Value&>(entry)));
                return { move(res_crs) };
            });
    };
    return visit(value);
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
