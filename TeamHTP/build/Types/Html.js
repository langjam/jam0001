import Base from './Base';
import { mdastToMd, mdToMdast } from "../Markdown";
class Html extends Base {
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
            const value = mdToMdast(text).children[0].value;
            this.getMdastContent().value = value;
        }
        return mdastToMd(this.getMdastContent(), true);
    }
}
export default Html;
