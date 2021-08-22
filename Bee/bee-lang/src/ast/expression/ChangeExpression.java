package ast.expression;

import token.Token;

public class ChangeExpression extends Expression {

    private final Token variableName;
    private final Expression value;

    public ChangeExpression(Token variableName, Expression value) {
        this.variableName = variableName;
        this.value = value;
    }

    public Token getVariableName() {
        return variableName;
    }

    public Expression getValue() {
        return value;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
