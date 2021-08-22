import Base from './Base'
import {Code as MdastCode} from 'mdast'
import {mdastToMd, mdToMdast} from "../Markdown";

class Code extends Base {
    getMdastContent(): MdastCode {
        return <MdastCode>this.mdastContent
    }

    append(item: string): void {
        let md = mdastToMd(this.getMdastContent(), true)
        md += item
        this.text(md)
    }

    text(text?: string): string {
        if (text) {
            this.getMdastContent().value = text
        }
        return this.getMdastContent().value
    }

    language(language?: string) {
        if (language) {
            this.getMdastContent().lang = language
        }
        return this.getMdastContent().lang
    }

    eval() {
        if (this.language() === 'js') {
            return eval(this.text())
        }
        else {
            console.log(`cannot eval non-js language '${this.language()}'`)
        }
    }
}

export default Code
