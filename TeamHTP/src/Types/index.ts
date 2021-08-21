import {Content as MdastContent, Definition as MdastDefinition} from 'mdast'

import Runtime from '../Runtime'

import Base from './Base'
import Blockquote from './Blockquote'
import Code from './Code'
import Definition from './Definition'
import Function from './Function'
import Heading from './Heading'
import Html from './Html'
import List from './List'
import Paragraph from './Paragraph'
import Table from './Table'
import Tag from './Tag'
import ThematicBreak from './ThematicBreak'

const TYPE_MAP: Record<string, typeof Base> = {
    'blockquote': Blockquote,
    'code': Code,
    'definition': Definition,
    'heading': Heading,
    'html': Html,
    'list': List,
    'paragraph': Paragraph,
    'table': Table,
    'thematicBreak': ThematicBreak,
}

function wrap(runtime: Runtime, mdastContent: MdastContent, parent?: Tag): Base {
    if (mdastContent.type === 'definition' && (mdastContent.url === ':' || !mdastContent.url.includes(':'))) {
        // Infer type of link definition in context of bubblegum
        if (mdastContent.label === null || mdastContent.label === undefined) {
            throw new Error('Tag name is empty')
        }
        const tagName = mdastContent.label
        const type = inferDefinitionType(mdastContent as MdastDefinition)
        if (type === Tag) {
            if (runtime.isTagDefined(tagName)) {
                throw new Error(`Tag with this name is already defined '${tagName}'`)
            }
            const tag = new (type as typeof Tag)(mdastContent, parent)
            runtime.defineTag(tagName, tag)
            return tag
        }
        else {
            // Associate function definition with parent tag
            if (runtime.isTagDefined(tagName)) {
                return new type(mdastContent, runtime.getTag(tagName))
            }
            else {
                throw new Error(`Reference to undefined tag '${tagName}'`)
            }
        }
    }
    else {
        // Some generic wrapping type
        const type = TYPE_MAP[mdastContent.type]
        if (type === undefined) {
            throw new Error(`mdast type not mapped '${mdastContent.type}'`)
        }
        return new type(mdastContent, parent)
    }
}

function inferDefinitionType(mdastDefinition: MdastDefinition): typeof Tag | typeof Function {
    if (mdastDefinition.title === null) {
        return Tag
    }
    else {
        return Function
    }
}

export {
    wrap,
    Base,
    Blockquote,
    Code,
    Function,
    Heading,
    Html,
    List,
    Paragraph,
    Table,
    Tag,
    ThematicBreak,
}
