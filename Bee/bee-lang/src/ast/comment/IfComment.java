package ast.comment;

import ast.expression.Expression;
import token.Token;

public class IfComment extends Comment {

    private final Token ifToken;
    private final Expression condition;
    private final BlockComment blockComment;

    public IfComment(Token ifToken, Expression condition, BlockComment block) {
        this.ifToken = ifToken;
        this.condition = condition;
        this.blockComment = block;
    }

    public Token getIfToken() {
        return ifToken;
    }

    public Expression getCondition() {
        return condition;
    }

    public BlockComment getBlockComment() {
        return blockComment;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
