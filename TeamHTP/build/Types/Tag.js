import Base from './Base';
class Tag extends Base {
    members;
    taggedElement;
    rawMd;
    constructor(mdastContent, rawMd, tag) {
        super(mdastContent, tag);
        this.taggedElement = undefined;
        this.rawMd = rawMd;
        this.members = {};
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
    isMemberDefined(memberName) {
        return this.members[memberName] !== undefined;
    }
    getMember(memberName) {
        return this.members[memberName];
    }
    addMember(memberName, memberFunction) {
        this.members[memberName] = memberFunction;
    }
    getRawMd() {
        return this.rawMd;
    }
}
export default Tag;
