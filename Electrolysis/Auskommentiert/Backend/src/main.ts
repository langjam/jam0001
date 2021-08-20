import {parse} from "./grammar";

let res = parse("rotate 2 comments above clockwise");
for(let e of res.errs) {
    console.log(e.toString());
}
console.log(JSON.stringify(res));