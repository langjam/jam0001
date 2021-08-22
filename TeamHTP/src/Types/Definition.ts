import Base from './Base'
import {Definition as MdastDefinition} from 'mdast'

class Definition extends Base {
    getMdastContent(): MdastDefinition {
        return <MdastDefinition>this.mdastContent
    }
}

export default Definition
