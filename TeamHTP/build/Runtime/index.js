class Runtime {
    tagDefs;
    constructor() {
        this.tagDefs = {};
    }
    isTagDefined(tagName) {
        return this.tagDefs[tagName] !== undefined;
    }
    defineTag(tagName, tag) {
        this.tagDefs[tagName] = tag;
    }
    getTag(tagName) {
        return this.tagDefs[tagName];
    }
}
export default Runtime;
