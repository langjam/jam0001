import express from 'express'
const app = express();
const port = 6789;



app.get("/api/data", (req : express.Request, res : express.Response) => {
    res.send("Hello World");
});