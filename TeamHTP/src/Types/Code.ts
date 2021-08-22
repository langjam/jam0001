import Base from './Base'
import {Code as MdastCode} from 'mdast'

class Code extends Base {
    getMdastContent(): MdastCode {
        return <MdastCode>this.mdastContent
    }
}

export default Code
