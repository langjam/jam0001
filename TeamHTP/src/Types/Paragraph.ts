import Base from './Base'
import {Paragraph as MdastParagraph} from 'mdast'
import {mdastToMd, mdToMdast} from "../Markdown";

class Paragraph extends Base {
    getMdastContent(): MdastParagraph {
        return <MdastParagraph>this.mdastContent
    }

    append(item: string): void {
        let md = mdastToMd(this.getMdastContent(), true)
        md += item
        this.text(md)
    }

    text(text?: string): string {
        if (text !== undefined) {
            const serializedChildren = (<MdastParagraph>mdToMdast(text).children[0]).children
            this.getMdastContent().children = serializedChildren
        }
        return mdastToMd(this.getMdastContent(), true)
    }
}

export default Paragraph
