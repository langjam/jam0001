import {Content, ListContent, Root} from 'mdast'
import {fromMarkdown} from 'mdast-util-from-markdown'
import {toMarkdown} from 'mdast-util-to-markdown'
import {gfm} from 'micromark-extension-gfm'
import {gfmFromMarkdown, gfmToMarkdown} from 'mdast-util-gfm'
import {Base, Function, Tag} from './Types'

function mdToMdast(src: string): Root {
    return fromMarkdown(src, {
        extensions: [gfm()],
        mdastExtensions: [gfmFromMarkdown]
    })
}

function mdastToMd(content: Content | Root, stripTrailingNewline = false): string {
    const md = toMarkdown(content, {extensions: [gfmToMarkdown()]})
    return md.substr(0, md.length - (stripTrailingNewline ? 1 : 0))
}

function wrappedElementToMd(content: Base): string {
    if (content instanceof Function) {
        return content.getRawMd()
    }
    if (content instanceof Tag) {
        return content.getRawMd()
    }
    return mdastToMd(content.getMdastContent())
}

function mdastListItemToMd(listItem: ListContent) {
    const listItemAsRoot: Root = {
        children: listItem.children,
        data: undefined,
        type: 'root'
    }
    return mdastToMd(listItemAsRoot).replace('\n', '')
}

export {
    mdastToMd,
    mdToMdast,
    mdastListItemToMd,
    wrappedElementToMd,
}
