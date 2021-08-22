import Tag from '../Types/Tag'
import {mdToMdast} from '../Markdown'
import {Base, Function, wrap} from '../Types'
import {Root} from 'mdast'
import {get_parser, Token, Tree} from "../mouthful"

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

const sortKeypairs = (pairs: [[string, string]]) => {
    const sorted = [...pairs].sort(([a, _a], [b, _b]) => {
        if (a < b) return -1
        if (a > b) return 1
        return 0
    })
    const name = sorted.map(([part,]) => part).join('$')
    const params = sorted.map(([, param]) => param).join(', ')
    return `${name}(${params})`
}

const transformer = {
    method: ([name, ...rest]: string[]) => `function ${name} {\n${rest.join('\n')}\n}`,
    binary_message: ([op, param]: [string, string]) => `${op}(${param})`,
    keyword_message: sortKeypairs,
    message_pair: ([key, value]: [string, string]) => [key.slice(0, -1), value],
    unary_message: ([name]: [string]) => `${name}()`,
    exprs: (exprs: string[]) => exprs.join('\n'),
    answer: ([v]: [string]) => `return ${v}`,
    assign: ([name, value]: [string, string]) => `${name} = ${value}`,
    keyword: ([callee, call, cascades]: [string, string, string[]]) => {
        const calls = [`${callee}.${call}`]
        for (const cascade of cascades) {
            calls.push(`${callee}.${cascade}`)
        }
        return `(${calls.join(', ')})`
    },
    keypairs: sortKeypairs,
    keypair: ([key, value]: [string, string]) => [key.slice(0, -1), value],
    cascades: (cascades: string[]) => cascades,
    binary: ([left, op, right]: [string, string, string]) => `${left}.${op}(${right})`,
    unary: ([left, op]: [string, string]) => `${left}.${op}()`,
    array: (array: string[]) => `[${array.join(', ')}]`,
    block: ([p, ...rest]: [Tree | string, ...string[]]) => {
        let params = '';
        let body = rest.join('\n');
        if (typeof p === 'string') {
            body = `${p}\n${body}`
        } else {
            const {children} = p
            params = children.join(', ')
        }
        return `function (${params}) {\n${body}\n}`
    },
    temps: (words: string[]) => `let ${words.join(', ')}`,
    map: (entries: string[]) => `{ ${entries.join(', ')} }`,
    mappair: ([keyword, value]: [string, string]) => `${keyword} ${value}`,
    scalar: ([value]: [string]) => `(${value})`,
    kw: ([{value}]: [Token]) => value as string,
    number: ([{value}]: Token[]) => `(${parseFloat(value)})`,
    op: ([{value}]: [Token]) => (value as string).split('').map(c => '$' + opNames[c]).join(''),
    param: ([{value}]: [Token]) => (value as string).substr(1),
    string: ([{value}]: [Token]) => value as string,
    symbol: ([{value}]: [Token]) => `Symbol('${value.substr(1)}')`,
    word: ([{value}]: [Token]) => value,
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
