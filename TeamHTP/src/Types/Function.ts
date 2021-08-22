import {Definition} from 'mdast'
import Base from './Base'
import Tag from './Tag'

class Function extends Base {
    private parent: Tag
    private rawMd: string

    constructor(mdastContent: Definition, parent: Tag, rawMd: string, tag?: Tag) {
        super(mdastContent, tag)
        this.parent = parent
        this.rawMd = rawMd
        console.log(this.rawMd)
    }

    getMdastContent(): Definition {
        return <Definition>this.mdastContent
    }

    getParent() {
        return this.parent
    }

    getName() {
        return this.getMdastContent().url
    }

    getBody() {
        return this.getMdastContent().title
    }

    getRawMd() {
        return this.rawMd
    }
}

export default Function
