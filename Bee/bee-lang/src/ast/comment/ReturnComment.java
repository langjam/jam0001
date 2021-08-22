package ast.comment;

import ast.expression.Expression;
import token.Token;

public class ReturnComment extends Comment {

    private final Token keyword;
    private final Expression value;

    public ReturnComment(Token keyword, Expression value) {
        this.keyword = keyword;
        this.value = value;
    }

    public Token getKeyword() {
        return keyword;
    }

    public Expression getValue() {
        return value;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
