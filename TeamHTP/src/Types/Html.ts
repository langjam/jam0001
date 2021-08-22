import Base from './Base'
import {HTML as MdastHtml} from 'mdast'
import {mdastToMd, mdToMdast} from "../Markdown";

class Html extends Base {
    getMdastContent(): MdastHtml {
        return <MdastHtml>this.mdastContent
    }

    append(item: string): void {
        let md = mdastToMd(this.getMdastContent(), true)
        md += item
        this.text(md)
    }

    text(text?: string): string {
        if (text !== undefined) {
            const value = (<MdastHtml>mdToMdast(text).children[0]).value
            this.getMdastContent().value = value
        }
        return mdastToMd(this.getMdastContent(), true)
    }
}

export default Html
