class Base {
    mdastContent;
    tag;
    constructor(mdastContent, tag) {
        this.mdastContent = mdastContent;
        this.tag = tag;
        console.log(`[${tag?.getMdastContent().label}]: ${mdastContent.type}`);
    }
    getMdastContent() {
        return this.mdastContent;
    }
}
export default Base;
