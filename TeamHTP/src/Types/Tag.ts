import Base from './Base'
import {Definition} from 'mdast'

class Tag extends Base {
    private taggedElement: Base | undefined
    private rawMd: string

    constructor(mdastContent: Definition, rawMd: string, tag?: Tag) {
        super(mdastContent, tag);
        this.taggedElement = undefined
        this.rawMd = rawMd
    }

    setTaggedElement(child: Base) {
        this.taggedElement = child
    }

    getTaggedElement(): Base | undefined {
        return this.taggedElement
    }

    getMdastContent(): Definition {
        return <Definition>this.mdastContent
    }

    getRawMd(): string {
        return this.rawMd
    }
}

export default Tag;
