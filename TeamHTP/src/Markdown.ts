import {Content, ListContent, Root} from 'mdast'
import {fromMarkdown} from 'mdast-util-from-markdown'
import {toMarkdown} from 'mdast-util-to-markdown'
import {gfm} from 'micromark-extension-gfm'
import {gfmFromMarkdown, gfmToMarkdown} from 'mdast-util-gfm'
import {Base} from './Types'

function mdToMdast(src: string): Root {
    return fromMarkdown(src, {
        extensions: [gfm()],
        mdastExtensions: [gfmFromMarkdown]
    })
}

function mdastToMd(content: Content | Root): string {
    return toMarkdown(content, {extensions: [gfmToMarkdown()]})
}

function wrappedElementToMd(content: Base): string {
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
}
