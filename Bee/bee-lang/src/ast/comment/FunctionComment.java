package ast.comment;

import token.Token;

import java.util.List;

public class FunctionComment extends Comment {

    private final Token name;
    private final List<Token> parameters;
    private final BlockComment blockComment;

    public FunctionComment(Token name, List<Token> parameters, BlockComment blockComment) {
        this.name = name;
        this.parameters = parameters;
        this.blockComment = blockComment;
    }

    public Token getName() {
        return name;
    }

    public List<Token> getParameters() {
        return parameters;
    }

    public BlockComment getBlockComment() {
        return blockComment;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
