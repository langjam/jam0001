import Tag from "../Types/Tag";

class Runtime {
    private readonly tagDefs: Record<string, Tag>

    constructor() {
        this.tagDefs = {}
    }

    isTagDefined(tagName: string) {
        return this.tagDefs[tagName] !== undefined
    }

    defineTag(tagName: string, tag: Tag) {
        this.tagDefs[tagName] = tag
    }

    getTag(tagName: string) {
        return this.tagDefs[tagName]
    }
}

export default Runtime
