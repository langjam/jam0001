import { CommentProvider } from "./CommentProvider";
import { Comment } from "./grammar";
import { WrappedComment, WrappedCommentSorter } from "./WrappedComment";
import { inspect } from "util";
import {parse} from "./grammar";
import { urlToHttpOptions } from "url";
import { compileFunction } from "vm";

export enum Direction {
    UP,
    DOWN
}

function ModelCommentSorter(a : ModelComment, b : ModelComment) : number {
    return a.date - b.date;
} 

class CommentBase {
    protected mId : string;
    protected mContent : string;
    protected mUpvotes : number;
    protected mChildren : ModelComment[];
    protected mDate : number;
    protected mParentId : string;
    constructor(id : string, parentId : string, content : string, upvotes : number, date : number, children: ModelComment[]) {
        this.mId = id;
        this.mParentId = parentId;
        this.mContent = content;
        this.mUpvotes = upvotes;
        this.mChildren = children;
        this.mDate = date;
    }
    get id() {
        return this.mId;
    }
    get content() {
        return this.mContent;
    }
    get upvotes() {
        return this.mUpvotes;
    }
    get children() {
        return this.mChildren;
    }
    get date() {
        return this.mDate;
    }
    get childrenSorted() {
        return this.mChildren.sort(ModelCommentSorter);
    }
    vote(value : number) {
        this.mUpvotes += value;
    }
    get parentId() {
        return this.mParentId;
    }
    addChild(child : ModelComment):void {
        this.mChildren.push(child);
    }
}


class ModelComment extends CommentBase {
    private mAST : Comment;
    constructor(ast : Comment, astString : string, upvotes : number, id : string, parentId : string, date: number, children : ModelComment[]) {
        super(id, parentId, astString, upvotes, date, children);
        this.mAST = ast;
    }

    toObject() : any {
        return {
            id: this.mId,
            content: this.mContent,
            children: this.mChildren.map(v => v.toObject()),
            upvotes: this.mUpvotes,
            date: this.mDate
        };
    }
    makeWrappedComment() {
        return new WrappedComment(this.mAST, this.mUpvotes, this.mId, this.mDate);
    }
}

class ModelPost extends CommentBase {
    private mTitle : string;
    get title() {
        return this.mTitle;
    }
    get id() {
        return this.mId;
    }
    get upvotes() {
        return this.mUpvotes;
    }
    constructor(title : string, content : string, id : string, parentId : string, upvotes : number, date : number, children : ModelComment[]) {
        super(id, parentId, content, upvotes, date, children);
        this.mTitle = title;
    }
    toObject() : any {
        return {
            id: this.mId,
            title: this.mTitle,
            content: this.mContent,
            children: this.childrenSorted.map(v => v.toObject()),
            upvotes: this.mUpvotes,
            date: this.mDate
        };
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
        return this.getFirstChildComment(this.mPostId);
    }
    getNextComment(currentCommentId: string): WrappedComment | undefined {
        let parentId = this.mModel.allCommentBases.get(currentCommentId)?.parentId;
        if(parentId === undefined) {
            return undefined;
        }
        let siblings = this.mModel.allCommentBases.get(parentId)?.childrenSorted;
        if(siblings === undefined) {
            return undefined;
        }
        for(let i = 0; i < siblings.length; ++i) {
            if(siblings[i].id === currentCommentId) {
                if(i + 1 >= siblings.length) {
                    return undefined;
                }
                return siblings[i + 1].makeWrappedComment();
            }
        }
        throw new Error("Comment not a child of it's own parent?!");
    }
    getFirstChildComment(parentCommentId: string): WrappedComment | undefined {
        return this.mModel.allCommentBases.get(parentCommentId)?.childrenSorted[0].makeWrappedComment();
    }
    getParentComment(childCommentId: string): WrappedComment | undefined {
        throw new Error("Method not implemented.");
    }
}

export class Model {
    private mCommentsMap : Map<string, CommentBase> = new Map();
    private mCounter : number = 1;
    addPost(post : any): void {
        let comments : ModelComment[] = [];
        for(let topLevelComment of post.children) {
            let comment = this.parseComment(post.id, topLevelComment);
            comments.push(comment);
        }
        this.mCommentsMap.set(post.id, new ModelPost(post.title, post.content, post.id, "", post.upvotes, post.date, comments));
        console.log(inspect(this.posts, false, null, true));
    }
    getNexUniqueId() : number {
        return this.mCounter++;
    }
    addComment(commentId : string, commentJson : any) {
        let commentObj : ModelComment = this.parseComment(commentId, commentJson)
        this.mCommentsMap.get(commentId)?.addChild(commentObj);
        this.mCommentsMap.set(commentObj.id, commentObj);
    }
    makeCommentProvider(postId : string) : CommentProvider {
        return new ModelCommentProvider(this, postId);
    }
    get posts() : ModelPost[] {
        return Array.from(this.mCommentsMap.values()).filter(c => c instanceof ModelPost) as ModelPost[];
    }
    get allCommentBases() : Map<string, CommentBase> {
        return this.mCommentsMap;
    }
    vote(id: string, value: number) {
        let component : CommentBase | undefined = this.mCommentsMap.get(id);
        component?.vote(value);
    }
    swapComments(id: string, direction : Direction) {

    }
    private parseComment(parentId : string, jsonComment : any) : ModelComment {
        let parseResult = parse(jsonComment.content);
        if(parseResult.errs.length > 0 || parseResult.ast === null) {
            console.log("HI");
            throw new Error(parseResult.errs[0].toString());
        }
        let comm = new ModelComment(
            parseResult.ast.comment,
            jsonComment.content, 
            jsonComment.upvotes, 
            jsonComment.id,
            parentId,
            jsonComment.date,
            jsonComment.children.map((c : any) => this.parseComment(jsonComment.id, c)));
        
        this.mCommentsMap.set(comm.id, comm);
        return comm;
    }
    toObject() {
        console.log(this.posts)
        return {
            topics: this.posts.map(post => post.toObject())
        }
    }
}