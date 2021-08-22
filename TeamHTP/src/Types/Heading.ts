import Base from './Base'
import {Heading as MdastHeading} from 'mdast'

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
}

export default Heading
