package ast.expression;

import token.Token;

public class SuperExpression extends Expression {

    private final Token superKeyword;
    private Token method;

    public SuperExpression(Token superKeyword, Token method) {
        this.superKeyword = superKeyword;
        this.method = method;
    }

    public Token getSuperKeyword() {
        return superKeyword;
    }

    public Token getMethod() {
        return method;
    }

    @Override
    public <R> R accept(ExpressionVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
