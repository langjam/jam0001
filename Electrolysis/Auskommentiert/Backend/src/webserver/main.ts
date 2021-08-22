import express from 'express';
import { Direction } from '../compiler/Model';
import {ApiModel} from './ApiModel';
import cors from 'cors';
import 'ws'
import * as http from 'http'
import WebSocket from 'ws';

const app = express();
const server = http.createServer(app);
const wss = new WebSocket.Server({server, path:'/api/ws/data'});
const port = 6789;
let api = new ApiModel();
app.use(express.json());
app.disable('etag');

app.use(cors());
app.get("/api/data", (req : express.Request, res : express.Response) => {
    api.getData(req, res);
});
app.post("/api/create_topic", (req : express.Request, res : express.Response) => {
    api.createTopic(req, res);
});
app.post("/api/create_comment", (req : express.Request, res : express.Response) => {
    //console.log(req.body)
    api.createComment(req, res);
});
app.post("/api/upvote", (req: express.Request, res: express.Response) => {
    api.upvote(req, res);
});
app.post("/api/downvote", (req: express.Request, res: express.Response) => {
    api.downvote(req, res);
});
app.post("/api/move_down", (req: express.Request, res: express.Response) => {
    api.swapComment(req, res, Direction.DOWN);
});
app.post("/api/move_up", (req: express.Request, res: express.Response) => {
    api.swapComment(req, res, Direction.UP);
});

app.get("/api/topic/:id", (req: express.Request, res: express.Response) => {
    api.queryTopic(req, res);
});

app.post("/api/delete", (req: express.Request, res: express.Response) => {
    api.delete(req, res);
});

wss.on('connection', (ws: WebSocket) => {
    api.registerCallback(ws);
    ws.on('close', () => {
        api.removeCallback(ws);
    })
});

server.listen(port, () => {
    console.log("api started");
});