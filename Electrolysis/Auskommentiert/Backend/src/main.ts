import {parse} from "./grammar";

let res = parse("x is 2+3 * 2");
for(let e of res.errs) {
    console.log(e.toString());
}
console.log(JSON.stringify(res));