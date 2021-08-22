import Tag from '../Types/Tag'
import {mdToMdast} from '../Markdown'
import {Base, Function, wrap} from '../Types'
import {Root} from 'mdast'
import {Position} from 'unist'

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
            this.wrappedElements.push(wrappedObject)
            if (wrappedObject instanceof Tag) {
                lastTag = wrappedObject
            }
            else {
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
