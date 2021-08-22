import Tag from '../Types/Tag';
import { mdToMdast } from '../Markdown';
import { wrap } from '../Types';
class Runtime {
    constructor(src) {
        this.tagDefs = {};
        this.docRoot = mdToMdast(src);
        this.wrappedElements = [];
        const srcLines = src.split('\n');
        let lastTag = undefined;
        for (const child of this.docRoot.children) {
            const position = child.position;
            let rawMd = '';
            if (position !== undefined) {
                rawMd = srcLines.slice(position.start.line - 1, position.end.line).join('\n');
            }
            const wrappedObject = wrap(this, child, rawMd, lastTag);
            this.wrappedElements.push(wrappedObject);
            if (wrappedObject instanceof Tag) {
                lastTag = wrappedObject;
            }
            else {
                lastTag = undefined;
            }
        }
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
    getDocRoot() {
        return this.docRoot;
    }
    getWrappedElements() {
        return this.wrappedElements;
    }
}
export default Runtime;
