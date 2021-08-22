import { fromMarkdown } from 'mdast-util-from-markdown';
import { toMarkdown } from 'mdast-util-to-markdown';
import { gfm } from 'micromark-extension-gfm';
import { gfmFromMarkdown, gfmToMarkdown } from 'mdast-util-gfm';
import { Function, Tag } from './Types';
function mdToMdast(src) {
    return fromMarkdown(src, {
        extensions: [gfm()],
        mdastExtensions: [gfmFromMarkdown]
    });
}
function mdastToMd(content, stripTrailingNewline = false) {
    const md = toMarkdown(content, { extensions: [gfmToMarkdown()] });
    return md.substr(0, md.length - (stripTrailingNewline ? 1 : 0));
}
function wrappedElementToMd(content) {
    if (content instanceof Function) {
        return content.getRawMd();
    }
    if (content instanceof Tag) {
        return content.getRawMd();
    }
    return mdastToMd(content.getMdastContent());
}
function mdastListItemToMd(listItem) {
    const listItemAsRoot = {
        children: listItem.children,
        data: undefined,
        type: 'root'
    };
    return mdastToMd(listItemAsRoot).replace('\n', '');
}
export { mdastToMd, mdToMdast, mdastListItemToMd, wrappedElementToMd, };
