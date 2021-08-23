import Base from './Base'
import {Table as MdastTable} from 'mdast'

class Table extends Base {
    getMdastContent(): MdastTable {
        return <MdastTable>this.mdastContent
    }
    // TODO: need more time!
}

export default Table
