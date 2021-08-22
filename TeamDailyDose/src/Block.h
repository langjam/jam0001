#pragma once

#include "Scope.h"
#include "Utils.h"
#include "Value.h"
#include <AK/NonnullRefPtr.h>
#include <AK/RefCounted.h>
#include <AK/Vector.h>

class StatementListNode;

class Block : public RefCounted<Block> {
    CONSTRUCT(Block)
public:
    Optional<Value> execute(NonnullRefPtr<Scope> calling_scope, Vector<Value> arguments);

private:
    Block(NonnullRefPtr<Scope> scope, Vector<String> arguments, NonnullRefPtr<StatementListNode> statements);

    NonnullRefPtr<Scope> m_scope;
    Vector<String> m_arguments;
    NonnullRefPtr<StatementListNode> m_statements;
};
