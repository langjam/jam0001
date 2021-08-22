package ast.expression;

import token.Token;

import java.util.List;

public class AsyncCallExpression extends Expression {

    private final Token callToken;
    private final Expression callee;
    private final List<Expression> arguments;

    public AsyncCallExpression(Token callToken, Expression callee, List<Expression> arguments) {
        this.callToken = callToken;
        this.callee = callee;
        this.arguments = arguments;
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

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
