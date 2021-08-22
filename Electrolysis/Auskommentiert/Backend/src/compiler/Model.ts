import { CommentProvider } from "./CommentProvider";
import { WrappedComment, WrappedCommentSorter } from "./WrappedComment";
import { inspect } from "util";

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
    set content(newContent : string) {
        this.mContent = newContent;
    }
    get upvotes() {
        return this.mUpvotes;
    }
    get children() {
        return this.mChildren;
    }
    set children(children : ModelComment[]) {
        this.mChildren = children;
    }
    get date() {
        return this.mDate;
    }
    set date(date) {
        this.mDate = date;
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
    set parentId(newParentId : string) {
        this.mParentId = newParentId;
    }
    addChild(child : ModelComment):void {
        this.mChildren.push(child);
    }
}


export class ModelComment extends CommentBase {
    constructor(astString : string, upvotes : number, id : string, parentId : string, date: number, children : ModelComment[]) {
        super(id, parentId, astString, upvotes, date, children);
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
        return new WrappedComment(this);
    }
}

export class ModelPost extends CommentBase {
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
    getPrevComment(currentCommentId: string): WrappedComment | undefined {
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
                if(i - 1 < 0) {
                    return undefined;
                }
                return siblings[i - 1].makeWrappedComment();
            }
        }
        throw new Error("Comment not a child of it's own parent?!");
    }
    getFirstChildComment(parentCommentId: string): WrappedComment | undefined {
        return this.mModel.allCommentBases.get(parentCommentId)?.childrenSorted[0].makeWrappedComment();
    }
    getParentComment(childCommentId: string): WrappedComment | undefined {
        let parentId = this.mModel.allCommentBases.get(childCommentId)?.parentId;
        if(parentId === undefined) {
            return undefined;
        }
        let parent = this.mModel.allCommentBases.get(parentId);
        if(parent === undefined) {
            return undefined;
        }
        if(parent instanceof ModelComment) {
            return (parent as ModelComment).makeWrappedComment();
        }
        return undefined;
    }

    moveCommentLeft(commentId : string) : void {
        let srcComment = this.mModel.allCommentBases.get(commentId);
        if(srcComment === undefined || !(srcComment instanceof ModelComment)) {
            throw new Error("Comment not found!");
        }
        let parentId = srcComment.parentId;
        if(parentId === undefined) {
            throw new Error("No parent!");
        }
        let parent = this.mModel.allCommentBases.get(parentId);
        if(parent === undefined) {
            throw new Error("Parent comment not found!");
        }
        let parentsParentId = parent.parentId;
        if(parentsParentId === undefined) {
            throw new Error("No parent's parent!");
        }
        let parentsParent = this.mModel.allCommentBases.get(parentsParentId);
        if(parentsParent === undefined) {
            throw new Error("Parent's parent nound found!");
        }
        let parentsSiblings = parentsParent.childrenSorted;
        for(let i = 0; i < parentsSiblings.length; ++i) {
            if(parentsSiblings[i].id === parentId) {
                parentsSiblings.splice(i + 1, 0, srcComment);
                parent.children.splice(parent.children.indexOf(srcComment), 1);
                srcComment.parentId = parentsParentId;
                this.mModel.notifyChange();
                break;
            }
        }
    }
    moveCommentRight(commentId : string) : void {

    }
    moveCommentUp(commentId : string) : void {

    }
    moveCommentDown(commentId : string) : void {

    }
    notifyContentChanged() : void {
        this.mModel.notifyChange();
    }
}

export class Model {
    private mCommentsMap : Map<string, CommentBase> = new Map();
    private mCounter : number = 1;
    private mCallback: Function = () => {}
    constructor(callback: Function = () => {}) {
        this.mCallback = callback;
    }
    addPost(post : any): void {
        let comments : ModelComment[] = [];
        for(let topLevelComment of post.children) {
            let comment = this.parseComment(post.id, topLevelComment);
            comments.push(comment);
        }
        let newPost = new ModelPost(post.title, post.content, post.id, "", post.upvotes, post.date, comments);
        this.mCommentsMap.set(post.id, newPost);
        this.mCallback();
    }
    getNexUniqueId() : string {
        return String(this.mCounter++);
    }
    addComment(commentId : string, commentJson : any) {
        let commentObj : ModelComment = this.parseComment(commentId, commentJson)
        this.mCommentsMap.get(commentId)?.addChild(commentObj);
        this.mCommentsMap.set(commentObj.id, commentObj);
        commentObj.parentId = commentId;
        this.mCallback();
    }
    makeCommentProvider(postId : string) : CommentProvider {
        console.log(this.mCommentsMap.get(postId));
        return new ModelCommentProvider(this, postId);
    }
    get posts() : ModelPost[] {
        return Array.from(this.mCommentsMap.values()).filter(c => c instanceof ModelPost) as ModelPost[];
    }
    get allCommentBases() : Map<string, CommentBase> {
        return this.mCommentsMap;
    }
    notifyChange() {
        this.mCallback();
    }
    vote(id: string, value: number) {
        let component : CommentBase | undefined = this.mCommentsMap.get(id);
        component?.vote(value);
    }
    swapFull(idFirst: string,  idSecond: string) {
        let entryOne = this.mCommentsMap.get(idFirst);
        let entryTwo = this.mCommentsMap.get(idSecond);

        if(entryOne !== undefined && entryTwo !== undefined) {
            let parentOne = this.mCommentsMap.get(entryOne.parentId);
            let parentTwo = this.mCommentsMap.get(entryTwo.parentId);

            if(parentOne !== undefined && parentTwo !== undefined) {
                let tempDate = entryOne.date;
                entryOne.date = entryTwo.date;
                entryTwo.date = tempDate;
                let tempParentId = entryOne.parentId;
                entryOne.parentId = entryTwo.parentId;
                entryTwo.parentId = tempParentId;

                parentOne.children.splice(parentOne.children.indexOf(entryOne as ModelComment));
                parentTwo.children.splice(parentTwo.children.indexOf(entryTwo as ModelComment));
                parentOne.children.push(entryTwo as ModelComment)
                parentTwo.children.push(entryOne as ModelComment)
            }
        }
        this.mCallback();
    }

    swapContent(idFirst: string, idSecond: string) {
        let entryOne = this.mCommentsMap.get(idFirst);
        let entryTwo = this.mCommentsMap.get(idSecond);

        if(entryOne !== undefined && entryTwo !== undefined) {
            let tempContent = entryOne.content;
            entryOne.content = entryTwo.content;
            entryTwo.content = tempContent;
        }
        this.mCallback();
    }

    private parseComment(parentId : string, jsonComment : any) : ModelComment {
        let comm = new ModelComment(
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
        //console.log(this.posts)
        return {
            topics: this.posts.map(post => post.toObject())
        }
    }
}