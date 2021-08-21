import { WrappedComment } from "./WrappedComment";

export abstract class CommentProvider {
    abstract getFirstComment(postId : string) : WrappedComment | null;
    abstract getNextComment(postId : string, currentCommentId : string) : WrappedComment | null;
    abstract getFirstChildComment(postId : string, parentCommentId : string) : WrappedComment | null;
    abstract getParentComment(postId : string, childCommentId : string) : WrappedComment | null;
}