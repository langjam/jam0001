import fs from 'fs'

import {startRepl} from './Repl'
import {List} from './Types'
import Runtime from './Runtime'
import {mdastToMd} from './Markdown'
import {saveToDisk} from './Runtime/File'
import transformer from "./transformer";
import {get_parser} from "./mouthful_repl";

const srcPath = process.argv[2];

const parser = get_parser({transformer})

console.log(srcPath)
const src = fs.readFileSync(srcPath).toString()
const runtime = new Runtime(src)

const list = <List>runtime.getTag('TodoList').getTaggedElement()
const updatedListItem = list.checkItem('**kick ass**')
if (updatedListItem !== undefined) {
    console.log(list.getMdastContent().position)
    console.log(mdastToMd(updatedListItem))
}

console.log('testing save')
saveToDisk(runtime)

startRepl((input) => {
    console.log(parser.parse(input))
}).catch(console.log)
