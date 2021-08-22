import fs from 'fs';
import { startRepl } from './Repl';
import Runtime from './Runtime';
import { saveToDisk } from './Runtime/File';
import { makeTransformer } from "./transformer";
import { get_parser, UnexpectedToken } from "./mouthful_repl";
Array.prototype['at'] = function (index) {
    return this[index];
};
Number.prototype['to'] = function (upTo) {
    return Array.from({ length: upTo - this }, (x, i) => i + this);
};
Object.prototype['get'] = function (name) {
    return this[name];
};
Object.prototype['set$to'] = function (name, to) {
    this[name] = to;
};
Boolean.prototype['not'] = function () {
    return !this;
};
const srcPath = process.argv[2];
//console.log(srcPath)
const src = fs.readFileSync(srcPath).toString();
const runtime = new Runtime(src);
startRepl(async (input) => {
    try {
        const evalScope = runtime.generateEvalScope();
        const transformer = makeTransformer(evalScope);
        const parser = get_parser({ transformer });
        const js = parser.parse(input);
        //console.log(js)
        const out = function (js) {
            return eval(js);
        }.call(evalScope, js);
        console.log(out);
        await saveToDisk(srcPath, runtime);
    }
    catch (e) {
        if (e instanceof UnexpectedToken) {
            console.log(`Syntax error! :(`);
        }
        else {
            throw e;
        }
    }
}).catch(console.log);
