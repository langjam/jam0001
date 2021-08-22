import fs from 'fs';
import { startRepl } from './Repl';
import Runtime from './Runtime';
import { mdastToMd } from './Markdown';
import { saveToDisk } from './Runtime/File';
import transformer from "./transformer";
import { get_parser } from "./mouthful_repl";
const srcPath = process.argv[2];
const parser = get_parser({ transformer });
console.log(srcPath);
const src = fs.readFileSync(srcPath).toString();
const runtime = new Runtime(src);
const list = runtime.getTag('TodoList').getTaggedElement();
const updatedListItem = list.checkItem('**kick ass**');
if (updatedListItem !== undefined) {
    console.log(list.getMdastContent().position);
    console.log(mdastToMd(updatedListItem));
}
console.log('testing save');
saveToDisk(runtime);
startRepl((input) => {
    const js = parser.parse(input);
    console.log(js);
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    // eslint-disable-next-line no-with
    with (runtime.generateEvalScope()) {
        eval(js);
    }
}).catch(console.log);
