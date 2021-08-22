import Base from './Base';
import { mdastToMd, mdToMdast } from "../Markdown";
class Paragraph extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    append(item) {
        let md = mdastToMd(this.getMdastContent(), true);
        md += item;
        const serializedChildren = mdToMdast(md).children[0].children;
        this.getMdastContent().children = serializedChildren;
    }
}
export default Paragraph;
