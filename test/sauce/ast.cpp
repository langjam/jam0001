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
    for (size_t i = 0; i < context.last_call_scope_start; ++i)
        scope.take_first();
    auto cscope = context.comment_scope;
    for (size_t i = 0; i < context.last_call_scope_start; ++i)
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
            auto last_scope = move(context.scope);
            auto last_cscope = move(context.comment_scope);

            auto last_stack_start = context.last_call_scope_start;
            context.last_call_scope_start = context.scope.size();

            context.scope.extend(ptr->scope);
            context.comment_scope.extend(ptr->comment_scope);

            context.scope.template empend();
            context.comment_scope.template empend();
            auto& scope = context.scope.last();

            if (ptr->node->return_())
                scope.set(ptr->node->return_()->name(), { Empty {} });

            size_t i = 0;
            for (auto& param : ptr->node->parameters()) {
                if (arguments.size() > i)
                    scope.set(param->name(), arguments[i]);
                else
                    scope.set(param->name(), { Empty {} });
                ++i;
            }

            auto old_comments = move(context.unassigned_comments);
            for (auto& node : ptr->node->body())
                node->run(context);

            Value result { Empty {} };
            if (ptr->node->return_())
                result = scope.get(ptr->node->return_()->name()).value();

            context.scope = move(last_scope);
            context.comment_scope = move(last_cscope);
            context.unassigned_comments = move(old_comments);
            context.last_call_scope_start = last_stack_start;

            return result;
        }
        if (auto ptr = callee.value.template get_pointer<NonnullRefPtr<Type>>()) {
            Type* type_ptr = ptr->ptr();
            if (auto type = type_ptr->decl.template get_pointer<NativeType>()) {
                if (arguments.is_empty()) {
                    return { Empty {} };
                }
                auto& first = flatten(arguments.first());
                switch (*type) {
                case NativeType::Any:
                    return first;
                case NativeType::Int:
                    if (first.value.template has<int>())
                        return first;
                    if (first.value.template has<String>())
                        return { first.value.template get<String>()[0] };
                case NativeType::String:
                    if (first.value.template has<String>())
                        return first;
                    if (first.value.template has<int>())
                        return { String::repeated(first.value.template get<int>(), 1) };
                }
                return { Empty {} };
            }

            auto& fields = type_ptr->decl.template get<Vector<TypeName>>();
            Vector<Value> values;
            bool did_initialize = false;
            if (arguments.size() > 0 && arguments.size() < fields.size()) {
                if (auto rv = arguments[0].value.template get_pointer<RecordValue>()) {
                    if (auto rfields = rv->type->decl.template get_pointer<Vector<TypeName>>()) {
                        if (rfields->size() >= fields.size()) {
                            size_t index = 0;
                            for (auto& entry : rv->members) {
                                values.append(
                                    create<Call>(
                                        static_ptr_cast<ASTNode>(create<SyntheticNode>(Value { fields[index].type })),
                                        Vector { static_ptr_cast<ASTNode>(create<SyntheticNode>(entry)) })
                                        ->run(context));
                                ++index;
                            }
                            did_initialize = true;
                        }
                    }
                }
            }

            if (!did_initialize) {
                size_t index = 0;
                for (auto& type_name : fields) {
                    if (arguments.size() <= index)
                        values.append({ Empty {} });
                    else
                        values.append(
                            create<Call>(
                                static_ptr_cast<ASTNode>(create<SyntheticNode>(Value { type_name.type })),
                                Vector { static_ptr_cast<ASTNode>(create<SyntheticNode>(arguments[index])) })
                                ->run(context));
                    ++index;
                }
            }
            return { RecordValue { *type_ptr, move(values) } };
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

        auto value = scope.find(m_name)->value;
        if (m_type) {
            value = create<Call>(
                *m_type,
                Vector { static_ptr_cast<ASTNode>(create<SyntheticNode>(value)) })
                ->run(context);
        }

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
    Function<Value(Value const&)> visit = [&](auto const& value) {
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
            [&](RecordValue const& rv) -> Value {
                if (rv.type->decl.template has<NativeType>())
                    return visit(rv.members.first());
                Optional<size_t> pindex;
                size_t index = 0;
                for (auto& entry : rv.type->decl.template get<Vector<TypeName>>()) {
                    if (entry.name == m_property) {
                        pindex = index;
                        break;
                    }
                    ++index;
                }
                if (!pindex.has_value())
                    return { Empty {} };

                return rv.members.at(*pindex);
            },
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
    if (m_variable->type()) {
        value = create<Call>(
                *const_cast<RefPtr<ASTNode>&>(m_variable->type()),
                Vector { static_ptr_cast<ASTNode>(create<SyntheticNode>(value)) })
                ->run(context);
    }
    context.scope.last().set(m_variable->name(), value);
    return value;
}
