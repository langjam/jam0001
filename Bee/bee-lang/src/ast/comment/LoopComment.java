package ast.comment;

import ast.expression.Expression;
import token.Token;

public class LoopComment extends Comment {

    private final Token loopToken;
    private final Expression condition;
    private final BlockComment blockComment;

    public LoopComment(Token loopToken, Expression condition, BlockComment blockComment) {
        this.loopToken = loopToken;
        this.condition = condition;
        this.blockComment = blockComment;
    }

    public Token getLoopToken() {
        return loopToken;
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
