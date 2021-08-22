import Base from './Base'
import {Code as MdastCode} from 'mdast'

class Code extends Base {
    getMdastContent(): MdastCode {
        return <MdastCode>this.mdastContent
    }

    append(item: string): void {
        let text = this.text()
        text += item
        this.text(text)
    }

    text(text?: string): string {
        if (text !== undefined) {
            this.getMdastContent().value = text
        }
        return this.getMdastContent().value
    }

    language(language?: string) {
        if (language !== undefined) {
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
