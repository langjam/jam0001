import Base from './Base'
import {Content, Heading as MdastHeading} from 'mdast'
import {mdastToMd, mdToMdast} from "../Markdown";

class Heading extends Base {
    getMdastContent(): MdastHeading {
        return <MdastHeading>this.mdastContent
    }

    depth(depth?: 1 | 2 | 3 | 4 | 5 | 6) {
        if (depth) {
            this.getMdastContent().depth = depth
        }
        return this.getMdastContent().depth
    }

    append(item: string): void {
        let md = this.text()
        md += item
        this.text(md)
    }

    text(text?: string): string {
        if (text !== undefined) {
            const serializedChildren = (<MdastHeading>mdToMdast(text).children[0]).children
            this.getMdastContent().children = serializedChildren
        }
        const contentOnly: Content = {
            children: this.getMdastContent().children, type: 'paragraph'
        }
        return mdastToMd(contentOnly, true)
    }
}

export default Heading
