package ast.expression;

import token.Token;

public class GetExpression extends Expression {

    private final Token name;
    private final Expression calle;

    public GetExpression(Token name, Expression calle) {
        this.name = name;
        this.calle = calle;
    }

    public Token getName() {
        return name;
    }

    public Expression getCalle() {
        return calle;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
