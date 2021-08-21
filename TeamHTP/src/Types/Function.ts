import {Definition} from 'mdast'
import Base from './Base'
import Tag from './Tag'

class Function extends Base {
    private parent: Tag

    constructor(mdastContent: Definition, parent: Tag, tag?: Tag) {
        super(mdastContent, tag)
        this.parent = parent
        console.log(`${this.getName()} (${this.getBody()})`)
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
}

export default Function
