import { Comment } from "./grammar";

export class WrappedComment {
    private mAST : Comment;
    private mUpvotes : number;
    private mId : string;
    private mDate : number;
    constructor(ast : Comment, upvotes : number, id : string, date : number) {
        this.mAST = ast;
        this.mUpvotes = upvotes;
        this.mId = id;
        this.mDate = date;
    }
    get ast() {
        return this.mAST;
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
    return a.date - b.date;
} 