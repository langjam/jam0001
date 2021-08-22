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
        const serializedChildren = (<MdastParagraph>mdToMdast(md).children[0]).children
        this.getMdastContent().children = serializedChildren
    }
}

export default Paragraph
