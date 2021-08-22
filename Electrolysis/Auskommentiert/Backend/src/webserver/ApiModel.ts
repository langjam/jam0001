import { Direction, Model } from "../compiler/Model";
import express from 'express'

export class ApiModel {
    private mModel : Model 

    constructor() {
        this.mModel = new Model();
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
        comment.id = this.mModel.getNexUniqueId();
        this.mModel.addComment(id, comment);
        res.sendStatus(200);
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
        this.mModel.swapComments(obj, direction);
        res.sendStatus(200);
    }
}