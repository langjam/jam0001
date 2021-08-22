import Base from './Base'
import Function from './Function'
import {Definition} from 'mdast'

class Tag extends Base {
    private readonly members: Record<string, Function>
    private taggedElement: Base | undefined
    private rawMd: string

    constructor(mdastContent: Definition, rawMd: string, tag?: Tag) {
        super(mdastContent, tag);
        this.taggedElement = undefined
        this.rawMd = rawMd
        this.members = {}
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

    isMemberDefined(memberName: string): boolean {
        return this.members[memberName] !== undefined
    }

    getMember(memberName: string): Function {
        return this.members[memberName]
    }

    addMember(memberName:string, memberFunction: Function): void {
        this.members[memberName] = memberFunction
    }

    getRawMd(): string {
        return this.rawMd
    }
}

export default Tag;
