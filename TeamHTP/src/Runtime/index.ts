import Tag from "../Types/Tag";

class Runtime {
    private readonly tagDefs: Record<string, Tag>

    constructor() {
        this.tagDefs = {}
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
