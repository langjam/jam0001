import { Direction, Model } from "../compiler/Model";
import express from 'express'
import 'ws'
import WebSocket from "ws";
import { VM } from "../compiler/VM";
import { WrappedComment } from "../compiler/WrappedComment";

export class ApiModel {
    private mModel : Model 
    private mCallbacks : Array<WebSocket> = [];

    constructor() {
        this.mModel = new Model(() => this.executeCallbacks());
        this.mCallbacks = [];
    }

    executeCallbacks() {
        for( let value of this.mCallbacks) {
            let obj = this.mModel.toObject();
            let jsonString = JSON.stringify(obj);
            value.send(jsonString);
        }
    }

    getData(req: express.Request, res: express.Response) {
        let obj = this.mModel.toObject();
        let jsonString = JSON.stringify(obj);
        res.send(jsonString);
    }

    createTopic(req: express.Request, res: express.Response) {
        let obj = req.body;
        let id = this.mModel.getNexUniqueId();
        obj.id = id;
        this.mModel.addPost(obj);
        res.sendStatus(200);
    }

    createComment(req: express.Request, res: express.Response) {
        let obj = req.body;
        let comment = obj.comment;
        let id = obj.parent;
        let posts = this.mModel.posts;
        if(comment.content === 'run') {
            let result = posts.filter( post => post.id === id)
            if(result.length > 0) {
                let vm = new VM(this.mModel.makeCommentProvider(id))
                vm.run().catch((e) => {
                    console.error(e);
                });
            }
            res.sendStatus(200);
        } else {
            comment.id = this.mModel.getNexUniqueId();
            this.mModel.addComment(id, comment);
            res.sendStatus(200);
        }
    }

    upvote(req: express.Request, res: express.Response) { 
        let obj = req.body;
        let id = obj.id;
        this.mModel.vote(id, 1);
        res.sendStatus(200);
    }

    downvote(req: express.Request, res: express.Response) { 
        let obj = req.body;
        let id = obj.id;
        this.mModel.vote(id, -1);
        res.sendStatus(200);
    }
    
    swapComment(req: express.Request, res: express.Response, direction : Direction) { 
        let obj = req.body;
        let commentProv = this.mModel.makeCommentProvider(obj.post);
        let other : WrappedComment | undefined = commentProv.getNextComment(obj.id);
        if(direction === Direction.UP) {
            other = commentProv.getPrevComment(obj.id);
        }
        //console.log(other)
        if(other !== undefined) {
            this.mModel.swapFull(obj.id, other.id);
        }
        res.sendStatus(200);
    }

    queryTopic(req: express.Request, res: express.Response) { 
        let results = this.mModel.posts.filter( value => value.id + "" === req.params.id)
        if(results.length > 0) {
            let jsonString = JSON.stringify(results[0].toObject());
            res.send(jsonString);
        } else {
            res.sendStatus(500);
        }
    }

    delete(req: express.Request, res: express.Response) {  
        let obj = req.body
        this.mModel.deleteComment(obj.id);
    }

    registerCallback(ws: WebSocket) {
        this.mCallbacks.push(ws);
    }
    removeCallback(ws: WebSocket ) {
        this.mCallbacks.splice(this.mCallbacks.indexOf(ws), 1);
    }
}