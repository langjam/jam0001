import Base from './Base'
import {Paragraph as MdastParagraph} from 'mdast'

class Paragraph extends Base {
    getMdastContent(): MdastParagraph {
        return <MdastParagraph>this.mdastContent
    }
}

export default Paragraph
