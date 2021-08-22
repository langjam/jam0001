import fs from 'fs'

import {startRepl} from './Repl'
import {List} from './Types'
import Runtime from './Runtime'
import {mdastToMd} from './Markdown'
import {saveToDisk} from './Runtime/File'
import {makeTransformer} from "./transformer";
import {get_parser} from "./mouthful_repl";

const srcPath = process.argv[2];

console.log(srcPath)
const src = fs.readFileSync(srcPath).toString()
const runtime = new Runtime(src)

const list = <List>runtime.getTag('TodoList').getTaggedElement()
const updatedListItem = list.checkItem('**kick ass**')
if (updatedListItem !== undefined) {
    console.log(list.getMdastContent().position)
    console.log(mdastToMd(updatedListItem))
}

startRepl(async (input) => {
    const evalScope = runtime.generateEvalScope()
    const transformer = makeTransformer(evalScope)
    const parser = get_parser({transformer})
    const js = parser.parse(input)
    console.log(js)
    const out = function (js: string) {
        return eval(js)
    }.call(evalScope, js)
    await saveToDisk(srcPath, runtime)
}).catch(console.log)
