import {Content, Definition} from 'mdast'
import Tag from './Tag'

class Base {
    protected mdastContent: Content
    protected tag?: Tag

    constructor(mdastContent: Content, tag?: Tag) {
        this.mdastContent = mdastContent
        this.tag = tag
        //console.log(`[${tag?.getMdastContent().label}]: ${mdastContent.type}`)
    }

    type() {
        return this.getMdastContent().type
    }

    getMdastContent(): Content {
        return this.mdastContent
    }
}

export default Base
