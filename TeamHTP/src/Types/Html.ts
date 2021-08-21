import Base from './Base'
import {HTML as MdastHtml} from "mdast";

class Html extends Base {
    getMdastContent(): MdastHtml {
        return <MdastHtml>this.mdastContent
    }
}

export default Html
