package ast.comment;

public abstract class Comment {
    public abstract <R> R accept(CommentVisitor<R> visitor);
}
