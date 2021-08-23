import Base from './Base'
import {ThematicBreak as MdastThematicBreak} from 'mdast'

class ThematicBreak extends Base {
    getMdastContent(): MdastThematicBreak {
        return <MdastThematicBreak>this.mdastContent
    }
}

export default ThematicBreak
