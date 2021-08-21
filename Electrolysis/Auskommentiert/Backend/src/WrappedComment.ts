import { Comment } from "./grammar";

export class WrappedComment {
    private mContent : Comment;
    private mUpvotes : number;
    private mId : string;
    private mDate : Date;
    constructor(content : Comment, upvotes : number, id : string, date : Date) {
        this.mContent = content;
        this.mUpvotes = upvotes;
        this.mId = id;
        this.mDate = date;
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
    get date() {
        return this.mDate;
    }
}

export function WrappedCommentSorter(a : WrappedComment, b : WrappedComment) : number {
    return a.date.getUTCMilliseconds() - b.date.getUTCMilliseconds()
} 