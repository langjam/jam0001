import { CommentProvider } from "./CommentProvider";
import { Comment } from "./grammar";
import { WrappedComment } from "./WrappedComment";
import { inspect } from "util";

class ModelComment extends WrappedComment {
    private children : WrappedComment[];
    constructor(content : Comment, upvotes : number, id : string, children : WrappedComment[]) {
        super(content, upvotes, id);
        this.children = children;
    }
}

class ModelPost {
    private mTitle : string;
    private mId : string;
    private mUpvotes : number;
    private mTopLevelComments : ModelComment[];
    get title() {
        return this.mTitle;
    }
    get topLevelComments() {
        return this.mTopLevelComments;
    }
    get id() {
        return this.mId;
    }
    get upvotes() {
        return this.mUpvotes;
    }
    constructor(title : string, id : string, upvotes : number, topLevelComments : ModelComment[]) {
        this.mTitle = title;
        this.mId = id;
        this.mUpvotes = upvotes;
        this.mTopLevelComments = topLevelComments;
    }
}

export class Model extends CommentProvider {
    private mPosts : ModelPost[] = [];

    addPost(post : any): void {
        let comments : ModelComment[] = [];
        for(let topLevelComment of post["topLevelComments"]) {
            comments.push(this.parseComment(topLevelComment));
        }
        this.mPosts.push(new ModelPost(post.title, post.id, post.upvotes, comments));
        console.log(inspect(this.mPosts, false, null, true));
    }
    private parseComment(jsonComment : any) : ModelComment {
        return new ModelComment(
            jsonComment["content"], 
            jsonComment["upvotes"], 
            jsonComment["id"], 
            jsonComment["children"].map((c : any) => this.parseComment(c)));
    }
    getFirstComment(postId : string): WrappedComment | null {
        throw new Error("Method not implemented.");
    }
    getNextComment(postId : string, currentCommentId: string): WrappedComment | null {
        throw new Error("Method not implemented.");
    }
    getFirstChildComment(postId : string, parentCommentId: string): WrappedComment | null {
        throw new Error("Method not implemented.");
    }
    getParentComment(postId : string, childCommentId: string): WrappedComment | null {
        throw new Error("Method not implemented.");
    }
}