import Base from './Base';
class Function extends Base {
    parent;
    rawMd;
    rawJs;
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
    setRawJs(rawJs) {
        this.rawJs = rawJs;
    }
    getRawJs() {
        return this.rawJs;
    }
    evalRawJs() {
        if (this.rawJs !== undefined) {
            return function (rawJs) {
                return eval(rawJs);
            }.call(this.getParent().getTaggedElement(), this.rawJs);
        }
    }
}
export default Function;
