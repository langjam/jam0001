package ast.comment;

public interface CommentVisitor<R> {
    R visit(CommentsUnit comment);
    R visit(EmptyComment comment);
    R visit(AlertComment comment);
    R visit(NoteComment comment);
    R visit(BlockComment comment);
    R visit(ExpressionComment comment);
    R visit(FunctionComment comment);
    R visit(IfComment comment);
    R visit(LetComment comment);
    R visit(LoopComment comment);
    R visit(ReturnComment comment);
    R visit(ControlFlowComment comment);
    R visit(AssertComment comment);
    R visit(TypeComment comment);
}
