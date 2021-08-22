class Base {
    mdastContent;
    tag;
    constructor(mdastContent, tag) {
        this.mdastContent = mdastContent;
        this.tag = tag;
        //console.log(`[${tag?.getMdastContent().label}]: ${mdastContent.type}`)
    }
    type() {
        return this.getMdastContent().type;
    }
    getMdastContent() {
        return this.mdastContent;
    }
    children() {
        return this.getMdastContent()['children'] ?? undefined;
    }
}
export default Base;
