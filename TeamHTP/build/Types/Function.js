import Base from './Base';
class Function extends Base {
    constructor(mdastContent, parent, rawMd, tag) {
        super(mdastContent, tag);
        this.parent = parent;
        this.rawMd = rawMd;
        console.log(this.rawMd);
    }
    getMdastContent() {
        return this.mdastContent;
    }
    getParent() {
        return this.parent;
    }
    getName() {
        return this.getMdastContent().url;
    }
    getBody() {
        return this.getMdastContent().title;
    }
    getRawMd() {
        return this.rawMd;
    }
}
export default Function;
