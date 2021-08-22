package ast.comment;

import ast.expression.Expression;
import token.Token;

public class AssertComment extends Comment {

    private final Token assertKeyword;
    private final Expression expression;

    public AssertComment(Token assertKeyword, Expression expression) {
        this.assertKeyword = assertKeyword;
        this.expression = expression;
    }

    public Token getAssertKeyword() {
        return assertKeyword;
    }

    public Expression getExpression() {
        return expression;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
