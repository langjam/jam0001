package ast.comment;

import token.Token;

public class ControlFlowComment extends Comment {

    private final Token keyword;
    private final ControlFlowType controlFlowType;

    public ControlFlowComment(Token keyword, ControlFlowType controlFlowType) {
        this.keyword = keyword;
        this.controlFlowType = controlFlowType;
    }

    public Token getKeyword() {
        return keyword;
    }

    public ControlFlowType getControlFlowType() {
        return controlFlowType;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
