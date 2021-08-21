import Base from './Base'
import {Blockquote as MdastBlockquote} from "mdast";

class Blockquote extends Base {
    getMdastContent(): MdastBlockquote {
        return <MdastBlockquote>this.mdastContent
    }
}

export default Blockquote
