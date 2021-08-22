import Base from './Base';
import { mdastToMd, mdToMdast } from "../Markdown";
class Blockquote extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    append(item) {
        let md = mdastToMd(this.getMdastContent(), true);
        md += item;
        this.text(md);
    }
    text(text) {
        if (text !== undefined) {
            const serializedChildren = mdToMdast(text).children[0].children;
            this.getMdastContent().children = serializedChildren;
        }
        return mdastToMd(this.getMdastContent(), true);
    }
}
export default Blockquote;
