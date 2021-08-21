import express from 'express'
const app = express();
const port = 6789;



app.get("/api/data", (req, res) => {
    res.send("Hello World");
});