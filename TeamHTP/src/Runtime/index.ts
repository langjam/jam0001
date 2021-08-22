import Tag from '../Types/Tag'
import {mdToMdast} from '../Markdown'
import {Base, Function, wrap} from '../Types'
import {Root} from 'mdast'
import {get_parser, Token} from "../mouthful"

const opNames: Record<string, string> = {
    "~": "tilde",
    "@": "at",
    "%": "percent",
    "&": "and",
    "*": "star",
    "-": "minus",
    "=": "equals",
    "+": "plus",
    "\\": "backslash",
    "|": "pipe",
    ",": "comma",
    "<": "less",
    ">": "greater",
    "/": "slash",
    "?": "question",
}

//
// 2 + 2
//
// 2.plus(2)
//

const transformer = {
    method: ([name, ...rest]: string[]) => `function ${name} {\n${rest.join('\n')}\n}`,
    binary_message([{value: op}, {value: param}]: Token[]) {
        const name = (op as string).split('').map(c => opNames[c]).join('$')
        return `${name}(${param})`
    },
    keyword_message: (pairs: [[string, string]]) => {
        const sorted = [...pairs].sort(([a, _a], [b, _b]) => {
            if (a < b) return -1
            if (a > b) return 1
            return 0
        })
        const name = sorted.map(([part, _]) => part).join('$')
        const params = sorted.map(([_, param]) => param).join(', ')
        return `${name}(${params})`
    },
    message_pair: ([{value: key}, {value}]: [Token, Token]) => {
        const namePart = (key as string).slice(0, -1) + (value as string)
        const param = value as string
        return [namePart, param]
    },
    unary_message: ([name]: Token[]) => `${name.value}()`,
    answer: ([v]: [string]) => `return ${v}`,
    temps: (words: Token[]) => `let ${words.map(w => w.value).join(', ')}`,
    number: ([n]: Token[]) => parseFloat(n.value),
}

const parser = get_parser({transformer})

class Runtime {
    private readonly tagDefs: Record<string, Tag>
    private readonly docRoot: Root
    private readonly wrappedElements: Base[]

    constructor(src: string) {
        this.tagDefs = {}

        this.docRoot = mdToMdast(src)
        this.wrappedElements = []

        const srcLines = src.split('\n')

        let lastTag: Tag | undefined = undefined
        for (const child of this.docRoot.children) {
            const position = child.position
            let rawMd = ''
            if (position !== undefined) {
                rawMd = srcLines.slice(position.start.line - 1, position.end.line).join('\n')
            }
            const wrappedObject = wrap(this, child, rawMd, lastTag)
            let body;
            if (wrappedObject instanceof Function && (body = wrappedObject.getBody())) {
                const result = parser.parse(body)
                console.log(result)
            }
            this.wrappedElements.push(wrappedObject)
            if (wrappedObject instanceof Tag) {
                lastTag = wrappedObject
            } else {
                lastTag = undefined
            }
        }
    }

    isTagDefined(tagName: string): boolean {
        return this.tagDefs[tagName] !== undefined
    }

    defineTag(tagName: string, tag: Tag): void {
        this.tagDefs[tagName] = tag
    }

    getTag(tagName: string): Tag {
        return this.tagDefs[tagName]
    }

    getDocRoot(): Root {
        return this.docRoot
    }

    getWrappedElements(): Base[] {
        return this.wrappedElements
    }
}

export default Runtime
