import Tag from '../Types/Tag'
import {mdToMdast} from '../Markdown'
import {wrap} from '../Types'
import {Root} from 'mdast'

class Runtime {
    private readonly tagDefs: Record<string, Tag>
    private readonly docRoot: Root

    constructor(src: string) {
        this.tagDefs = {}

        this.docRoot = mdToMdast(src)

        let lastTag: Tag | undefined = undefined
        for (const child of this.docRoot.children) {
            const wrappedObject = wrap(this, child, lastTag)
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
}

export default Runtime
