import { Comment } from "./grammar";

export class WrappedComment {
    private mContent : Comment;
    private mUpvotes : number;
    private mId : string;
    constructor(content : Comment, upvotes : number, id : string) {
        this.mContent = content;
        this.mUpvotes = upvotes;
        this.mId = id;
    }
    get content() {
        return this.mContent;
    }
    get upvotes() {
        return this.mUpvotes;
    }
    get id() {
        return this.mId;
    }
}