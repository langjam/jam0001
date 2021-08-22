package ast.comment;

import token.Token;

public class NoteComment extends Comment {

    private final Token note;

    public NoteComment(Token note) {
        this.note = note;
    }

    public Token getNote() {
        return note;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}
