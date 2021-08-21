import { CommentProvider } from "./CommentProvider";
import { Comment } from "./grammar";
import { WrappedComment, WrappedCommentSorter } from "./WrappedComment";
import { inspect } from "util";
import {parse} from "./grammar";

class ModelComment extends WrappedComment {
    private children : WrappedComment[];
    private astString : string;
    constructor(ast : Comment, astString : string, upvotes : number, id : string, date: Date, children : WrappedComment[]) {
        super(ast, upvotes, id, date);
        this.children = children;
        this.astString = astString;
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
    get topLevelCommentsSorted() {
        return this.mTopLevelComments.sort(WrappedCommentSorter);
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

class ModelCommentProvider extends CommentProvider {
    private mModel : Model;
    private mPostId : string;
    constructor(model : Model, postId : string) {
        super();
        this.mModel = model;
        this.mPostId = postId;
    }
    getFirstComment(): WrappedComment | undefined {
        return this.mModel.posts.get(this.mPostId)?.topLevelCommentsSorted[0];
    }
    getNextComment(currentCommentId: string): WrappedComment | undefined {
        throw new Error("Method not implemented.");
    }
    getFirstChildComment(parentCommentId: string): WrappedComment | undefined {
        throw new Error("Method not implemented.");
    }
    getParentComment(childCommentId: string): WrappedComment | undefined {
        throw new Error("Method not implemented.");
    }
}

export class Model {
    private mPosts : Map<string, ModelPost> = new Map();
    addPost(post : any): void {
        let comments : ModelComment[] = [];
        for(let topLevelComment of post.comments) {
            comments.push(this.parseComment(topLevelComment));
        }
        this.mPosts.set(post.id, new ModelPost(post.title, post.id, post.upvotes, comments));
        console.log(inspect(this.mPosts, false, null, true));
    }
    addComment(postId : string, comment : ModelComment) {

    }
    makeCommentProvider(postId : string) : CommentProvider {
        return new ModelCommentProvider(this, postId);
    }
    get posts() {
        return this.mPosts;
    }
    private parseComment(jsonComment : any) : ModelComment {
        let parseResult = parse(jsonComment.content);
        if(parseResult.errs.length > 0 || parseResult.ast === null) {
            console.log("HI");
            throw new Error(parseResult.errs[0].toString());
        }
        return new ModelComment(
            parseResult.ast.comment,
            jsonComment.content, 
            jsonComment.upvotes, 
            jsonComment.id,
            jsonComment.date,
            jsonComment.children.map((c : any) => this.parseComment(c)));
    }
}