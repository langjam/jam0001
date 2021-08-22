import fs from 'fs'

import {startRepl} from './Repl'
import {List} from './Types'
import Runtime from './Runtime'
import {mdastToMd} from './Markdown'
import {saveToDisk} from './Runtime/File'

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

console.log('testing save')
saveToDisk(runtime)

startRepl((input) => {
    console.log(input)
}).catch(console.log)
