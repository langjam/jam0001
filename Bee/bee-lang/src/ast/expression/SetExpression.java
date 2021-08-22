package ast.expression;

import token.Token;

public class SetExpression extends Expression {

    private final Expression object;
    private final Token name;
    private final Expression value;

    public SetExpression(Expression object, Token name, Expression value) {
        this.object = object;
        this.name = name;
        this.value = value;
    }

    public Expression getObject() {
        return object;
    }

    public Token getName() {
        return name;
    }

    public Expression getValue() {
        return value;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
