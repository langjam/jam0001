package ast.comment;

import ast.expression.Expression;
import token.Token;

public class LetComment extends Comment {

    private final Token variableName;
    private final Expression value;

    public LetComment(Token variableName, Expression value) {
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
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
