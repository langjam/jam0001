package ast.comment;

public class EmptyComment extends Comment {

    private static EmptyComment mInstance;

    private EmptyComment() {}

    public static synchronized EmptyComment getInstance() {
        if(mInstance == null) mInstance = new EmptyComment();
        return mInstance;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
