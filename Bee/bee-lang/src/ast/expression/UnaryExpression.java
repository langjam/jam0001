package ast.expression;

import token.Token;

public class UnaryExpression extends Expression {

    private final Expression right;
    private final Token operator;

    public UnaryExpression(Expression right, Token operator) {
        this.right = right;
        this.operator = operator;
    }

    public Expression getRight() {
        return right;
    }

    public Token getOperator() {
        return operator;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
