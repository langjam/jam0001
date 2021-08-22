import { Comment, parse } from "./grammar";
import {ModelComment} from "./Model";

export class WrappedComment {
    private mOriginalComment : ModelComment;
    constructor(comment : ModelComment) {
        this.mOriginalComment = comment;
    }
    parseAST() : Comment {
        let res = parse(this.content);
        let ast = res.ast?.comment;
        if(ast === null || ast === undefined) {
            throw new Error("Can't parse comment '" + this.content + "': " + res.errs[0].toString());
        }
        return ast;
    }
    get upvotes() {
        return this.mOriginalComment.upvotes;
    }
    get id() {
        return this.mOriginalComment.id;
    }
    get date() {
        return this.mOriginalComment.date;
    }
    get content() {
        return this.mOriginalComment.content;
    }
    set content(newContent : string) {
        this.mOriginalComment.content = newContent;
    }
}

export function WrappedCommentSorter(a : WrappedComment, b : WrappedComment) : number {
    return a.date - b.date;
} 