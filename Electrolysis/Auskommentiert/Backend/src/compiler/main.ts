
import {VM} from "./VM";
import {Model} from "./Model";
import * as readline from "readline";

let model = new Model();
model.addPost({
    title: "main",
    id: "22",
    upvotes: 4,
    date: 100,
    children: [
        {
            id: "31",
            content: "x is 0",
            upvotes: -5,
            date: 100,
            children: []
        },
        {
            id: "32",
            content: "while x < 10:",
            upvotes: 1,
            date: 200,
            children: [
                {
                    id: "36",
                    content: "if x<=5:",
                    upvotes: -5,
                    date: 300,
                    children: [
                        {
                            id: "33",
                            content: "log(x, sqrt(x))",
                            upvotes: -5,
                            date: 400,
                            children: []
                        }
                    ]
                },
                {
                    id: "35",
                    content: "x is x + 1",
                    upvotes: -5,
                    date: 400,
                    children: []
                }
            ]
        }
    ]
});
let vm = new VM(model.makeCommentProvider("22"));
vm.run();
