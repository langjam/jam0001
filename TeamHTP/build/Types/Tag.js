import Base from './Base';
class Tag extends Base {
    taggedElement;
    rawMd;
    constructor(mdastContent, rawMd, tag) {
        super(mdastContent, tag);
        this.taggedElement = undefined;
        this.rawMd = rawMd;
    }
    setTaggedElement(child) {
        this.taggedElement = child;
    }
    getTaggedElement() {
        return this.taggedElement;
    }
    getMdastContent() {
        return this.mdastContent;
    }
    getRawMd() {
        return this.rawMd;
    }
}
export default Tag;
