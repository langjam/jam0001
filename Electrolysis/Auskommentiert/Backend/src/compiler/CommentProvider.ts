import { WrappedComment } from "./WrappedComment";

export abstract class CommentProvider {
    abstract getFirstComment() : WrappedComment | undefined;
    abstract getPrevComment(currentCommentId : string) : WrappedComment | undefined;
    abstract getNextComment(currentCommentId : string) : WrappedComment | undefined;
    abstract getFirstChildComment(parentCommentId : string) : WrappedComment | undefined;
    abstract getParentComment(childCommentId : string) : WrappedComment | undefined;

    abstract moveCommentLeft(commentId : string) : void;
    abstract moveCommentRight(commentId : string) : void;
    abstract moveCommentUp(commentId : string) : void;
    abstract moveCommentDown(commentId : string) : void;

    abstract notifyContentChanged() : void;
    abstract swapFull(idFirstComment: string, idSecondComment: string) : void;
    abstract swapContent(idFirstComment: string, idSecondComment: string): void;
}