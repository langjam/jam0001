import Base from './Base'
import {Heading as MdastHeading} from "mdast";

class Heading extends Base {
    getMdastContent(): MdastHeading {
        return <MdastHeading>this.mdastContent
    }
}

export default Heading
