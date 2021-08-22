import Tag from '../Types/Tag';
import { mdToMdast } from '../Markdown';
import { Function, wrap } from '../Types';
import { get_parser, UnexpectedToken } from "../mouthful";
import { makeTransformer } from "../transformer";
const transformer = makeTransformer({});
const parser = get_parser({ transformer });
Array.prototype['at'] = function (index) {
    return this[index];
};
Number.prototype['to'] = function (upTo) {
    return Array.from({ length: upTo - this }, (x, i) => i + this);
};
Boolean.prototype['not'] = function () {
    return !this;
};
Boolean.prototype['ifTrue'] = function (then) {
    if (this)
        return then();
};
Boolean.prototype['ifFalse'] = function (then) {
    if (!this)
        return then();
};
Boolean.prototype['ifFalse$ifTrue'] = function (f, t) {
    if (this)
        return t();
    else
        return f();
};
Object['from$get'] = (from, get) => from[get];
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
                // eslint-disable-next-line no-useless-catch
                try {
                    const result = parser.parse(body);
                    //console.log(result)
                    wrappedObject.setRawJs(result);
                    wrappedObject.evalRawJs();
                }
                catch (e) {
                    if (e instanceof UnexpectedToken) {
                        console.log(`Syntax error on line ${wrappedObject.getMdastContent().position.start.line + e.token.line - 1}! :(`);
                    }
                    else {
                        throw e;
                    }
                }
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
