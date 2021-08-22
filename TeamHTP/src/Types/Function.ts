import {Definition} from 'mdast'
import Base from './Base'
import Tag from './Tag'

class Function extends Base {
    private parent: Tag
    private rawMd: string
    private rawJs: string | undefined

    constructor(mdastContent: Definition, parent: Tag, rawMd: string, tag?: Tag) {
        super(mdastContent, tag)
        this.parent = parent
        this.rawMd = rawMd
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

    setRawJs(rawJs:string) {
        this.rawJs = rawJs
    }

    getRawJs(): string | undefined {
        return this.rawJs
    }

    evalRawJs() {
        if (this.rawJs !== undefined) {
            return function (rawJs) {
                eval(`this['${rawJs.name}'] = ${rawJs.func}`)
            }.call(this.getParent().getTaggedElement(), this.rawJs)
        }
    }
}

export default Function
