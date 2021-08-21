import {Content, ListContent, Root} from 'mdast'
import {unified} from "unified";
import remarkParse from "remark-parse";
import remarkGfm from "remark-gfm";
import {toMarkdown} from 'mdast-util-to-markdown'
import {gfmToMarkdown} from 'mdast-util-gfm'

function mdToMdast(src: string): Root {
    return unified()
        .use(remarkParse)
        .use(remarkGfm)
        .parse(src)
}

function mdastToMd(content: Content | Root): string {
    return toMarkdown(content, {extensions: [gfmToMarkdown()]})
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
