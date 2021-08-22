import Tag from '../Types/Tag'
import {mdToMdast} from '../Markdown'
import {Base, Function, wrap} from '../Types'
import {Root} from 'mdast'
import {get_parser} from "../mouthful"
import {makeTransformer} from "../transformer";

const transformer = makeTransformer({})
const parser = get_parser({transformer})

class Runtime {
    private readonly evalScope: Record<string, Base>
    private readonly tagDefs: Record<string, Tag>
    private readonly docRoot: Root
    private readonly wrappedElements: Base[]

    constructor(src: string) {
        this.evalScope = {}
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
                //console.log(result)
                wrappedObject.setRawJs(result)
                wrappedObject.evalRawJs()
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

    generateEvalScope(): Record<string, Base> {
        const scope: Record<string, Base> = {}
        for (const tagName in this.tagDefs) {
            const taggedElement = this.tagDefs[tagName].getTaggedElement()
            if (taggedElement !== undefined) {
                scope[tagName] = taggedElement
            }
        }
        return scope
    }

}

export default Runtime
