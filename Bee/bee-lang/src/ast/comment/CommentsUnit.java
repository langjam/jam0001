package ast.comment;

import java.util.List;

public class CommentsUnit extends Comment {

    private final List<Comment> comments;

    public CommentsUnit(List<Comment> comments) {
        this.comments = comments;
    }

    public List<Comment> getComments() {
        return comments;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
