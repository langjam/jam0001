
import {VM} from "./VM";
import {Model} from "./Model";
import * as readline from "readline";

let model = new Model();
model.addPost({
    title: "main",
    id: "22",
    upvotes: 4,
    date: Date.now(),
    comments: [
        {
            id: "t6_1",
            content: "while 3 < 5 + 2:",
            upvotes: 1,
            date: Date.now(),
            children: [
                {
                    id: "31",
                    content: "log(5)",
                    upvotes: -5,
                    date: Date.now(),
                    children: []
                }
            ]
        }
    ]
});
let vm = new VM(model.makeCommentProvider("22"));
vm.run();
