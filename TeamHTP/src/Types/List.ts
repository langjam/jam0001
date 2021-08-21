import Base from './Base'
import {List as MdastList} from "mdast";

class List extends Base {
    getMdastContent(): MdastList {
        return <MdastList>this.mdastContent
    }
}

export default List
