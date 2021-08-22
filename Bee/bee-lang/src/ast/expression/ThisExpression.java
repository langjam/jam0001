package ast.expression;

import token.Token;

public class ThisExpression extends Expression {

    private final Token thisKeyword;

    public ThisExpression(Token thisKeyword) {
        this.thisKeyword = thisKeyword;
    }

    public Token getThisKeyword() {
        return thisKeyword;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
