import Base from './Base';
class Tag extends Base {
    constructor(mdastContent, rawMd, tag) {
        super(mdastContent, tag);
        this.child = undefined;
        this.rawMd = rawMd;
        this.members = {};
    }
    setChild(child) {
        this.child = child;
    }
    getChild() {
        return this.child;
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
