import {parse} from "./grammar";
import {VM} from "./VM";
import {Model} from "./Model";
import * as readline from "readline";

let res = parse("x is 2+3 * 2");
for(let e of res.errs) {
    console.log(e.toString());
}
let model = new Model();
model.addPost({
    title: "main",
    id: "22",
    upvotes: 4,
    topLevelComments: [
        {
            id: "1",
            content: "while true:",
            upvotes: 1,
            children: [

            ]
        }
    ]
});
if(res.ast !== null) {
    let vm = new VM(model.makeCommentProvider("22"));
    vm.run();
}