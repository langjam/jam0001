package ast.expression;

import token.Token;

import java.util.List;

public class CallExpression extends Expression {

    private final Token callToken;
    private final Expression callee;
    private final List<Expression> arguments;
    private final CallType callType;

    public CallExpression(Token callToken, Expression callee, List<Expression> arguments, CallType callType) {
        this.callToken = callToken;
        this.callee = callee;
        this.arguments = arguments;
        this.callType = callType;
    }

    public Token getCallToken() {
        return callToken;
    }

    public Expression getCallee() {
        return callee;
    }

    public List<Expression> getArguments() {
        return arguments;
    }

    public CallType getCallType() {
        return callType;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
