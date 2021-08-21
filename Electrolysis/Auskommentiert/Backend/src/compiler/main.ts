
import {VM} from "./VM";
import {Model} from "./Model";
import * as readline from "readline";

let model = new Model();
model.addPost({
    title: "main",
    id: "t2_22",
    upvotes: 4,
    date: Date.now(),
    comments: [
        {
            id: "t6_1",
            content: "while true:",
            upvotes: 1,
            date: Date.now(),
            children: [

            ]
        }
    ]
});
let vm = new VM(model.makeCommentProvider("22"));
vm.run();
