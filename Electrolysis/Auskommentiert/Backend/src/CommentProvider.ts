import { WrappedComment } from "./WrappedComment";

export abstract class CommentProvider {
    abstract getFirstComment() : WrappedComment | undefined;
    abstract getNextComment(currentCommentId : string) : WrappedComment | undefined;
    abstract getFirstChildComment(parentCommentId : string) : WrappedComment | undefined;
    abstract getParentComment(childCommentId : string) : WrappedComment | undefined;
}