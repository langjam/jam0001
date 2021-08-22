import Tag from '../Types/Tag';
import { mdToMdast } from '../Markdown';
import { Function, wrap } from '../Types';
import { get_parser } from "../mouthful";
import transformer from "../transformer";
const parser = get_parser({ transformer });
class Runtime {
    evalScope;
    tagDefs;
    docRoot;
    wrappedElements;
    constructor(src) {
        this.evalScope = {};
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
            let body;
            if (wrappedObject instanceof Function && (body = wrappedObject.getBody())) {
                const result = parser.parse(body);
                console.log(result);
                wrappedObject.setRawJs(result);
                wrappedObject.evalRawJs();
            }
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
    generateEvalScope() {
        const scope = {};
        for (const tagName in this.tagDefs) {
            const taggedElement = this.tagDefs[tagName].getTaggedElement();
            if (taggedElement !== undefined) {
                scope[tagName] = taggedElement;
            }
        }
        return scope;
    }
}
export default Runtime;
