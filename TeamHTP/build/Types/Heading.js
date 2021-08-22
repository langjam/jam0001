import Base from './Base';
import { mdastToMd, mdToMdast } from "../Markdown";
class Heading extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    depth(depth) {
        if (depth) {
            this.getMdastContent().depth = depth;
        }
        return this.getMdastContent().depth;
    }
    append(item) {
        let md = this.text();
        md += item;
        this.text(md);
    }
    text(text) {
        if (text !== undefined) {
            const serializedChildren = mdToMdast(text).children[0].children;
            this.getMdastContent().children = serializedChildren;
        }
        const contentOnly = {
            children: this.getMdastContent().children, type: 'paragraph'
        };
        return mdastToMd(contentOnly, true);
    }
}
export default Heading;
