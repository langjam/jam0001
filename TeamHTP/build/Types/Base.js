class Base {
    constructor(mdastContent, tag) {
        this.mdastContent = mdastContent;
        this.tag = tag;
        console.log(`[${tag === null || tag === void 0 ? void 0 : tag.getMdastContent().label}]: ${mdastContent.type}`);
    }
    getMdastContent() {
        return this.mdastContent;
    }
}
export default Base;
