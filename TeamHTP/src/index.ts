import {unified} from 'unified'
import remarkParse from 'remark-parse'
import remarkGfm from 'remark-gfm'
import {startRepl} from './Repl'
import {wrap, Tag} from './Types'
import Runtime from './Runtime'

const src = `
# Things I'm here to do
[TodoList]::
- [ ] kick ass
- [ ] chew bubble gum

[Heading]::
# Things I have
- [ ] bubble gum

\`\`\`bubbglegum
def check(n) {
    test
}
\`\`\`

[TodoList]: kickass (
  [self CheckItem:"kick ass"]
  if false or \${true and true} or true {
    a
    !@#$%^&*
    {}:"<>?|\`~\\\\,./
  }
)
`

// [TodoList kickass]
// [self ayy]

const runtime = new Runtime()

const docTree = unified()
    .use(remarkParse)
    .use(remarkGfm)
    .parse(src)

let lastTag: Tag | undefined = undefined
for (const child of docTree.children) {
    const wrappedObject = wrap(runtime, child, lastTag)
    if (wrappedObject instanceof Tag) {
        lastTag = wrappedObject
    }
    else {
        lastTag = undefined
    }
}

console.log(runtime)

startRepl((input) => {
    console.log(input)
}).catch(console.log)
