import Base from './Base'
import {Definition} from 'mdast'

class Tag extends Base {
    constructor(mdastContent: Definition, tag?: Tag) {
        super(mdastContent, tag);
    }

    getMdastContent(): Definition {
        return <Definition>this.mdastContent
    }
}

export default Tag;
