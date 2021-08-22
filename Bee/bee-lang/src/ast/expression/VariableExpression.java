package ast.expression;

import token.Token;

public class VariableExpression extends Expression{

    private final Token variable;

    public VariableExpression(Token variable) {
        this.variable = variable;
    }

    public Token getVariable() {
        return variable;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
