import Base from './Base'
import {Blockquote as MdastBlockquote} from 'mdast'
import {mdastToMd, mdToMdast} from "../Markdown";

class Blockquote extends Base {
    getMdastContent(): MdastBlockquote {
        return <MdastBlockquote>this.mdastContent
    }

    append(item: string): void {
        let md = mdastToMd(this.getMdastContent(), true)
        md += item
        this.text(md)
    }

    text(text?: string): string {
        if (text !== undefined) {
            const serializedChildren = (<MdastBlockquote>mdToMdast(text).children[0]).children
            this.getMdastContent().children = serializedChildren
        }
        return mdastToMd(this.getMdastContent(), true)
    }
}

export default Blockquote
