import Base from './Base';
class Function extends Base {
    parent;
    constructor(mdastContent, parent, tag) {
        super(mdastContent, tag);
        this.parent = parent;
        console.log(`${this.getName()} (${this.getBody()})`);
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
}
export default Function;
