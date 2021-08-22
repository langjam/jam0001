package ast.comment;

import ast.expression.Expression;

public class ExpressionComment extends Comment {

    private final Expression expression;

    public ExpressionComment(Expression expression) {
        this.expression = expression;
    }

    public Expression getExpression() {
        return expression;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
