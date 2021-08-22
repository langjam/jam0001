import express from 'express';
import { Direction } from '../compiler/Model';
import {ApiModel} from './ApiModel';
import cors from 'cors';
import bodyParser from 'body-parser';

const app = express();
const port = 6789;
let api = new ApiModel();
app.use(bodyParser.json());
app.disable('etag');

app.use(cors());
app.get("/api/data", (req : express.Request, res : express.Response) => {
    api.getData(req, res);
});
app.post("/api/create_topic", (req : express.Request, res : express.Response) => {
    api.createTopic(req, res);
});
app.post("/api/create_comment", (req : express.Request, res : express.Response) => {
    console.log(req.body)
    api.createComment(req, res);
});
app.post("/api/upvote", (req: express.Request, res: express.Response) => {
    api.upvote(req, res);
});
app.post("/api/downvote", (req: express.Request, res: express.Response) => {
    api.downvote(req, res);
});
app.post("/api/swap_down", (req: express.Request, res: express.Response) => {
    api.swapComment(req, res, Direction.DOWN);
});
app.post("/api/downvote", (req: express.Request, res: express.Response) => {
    api.swapComment(req, res, Direction.UP);
});

app.listen(port, () => {
    console.log("api started");
})