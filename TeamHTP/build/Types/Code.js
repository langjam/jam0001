import Base from './Base';
class Code extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    append(item) {
        let text = this.text();
        text += item;
        this.text(text);
    }
    text(text) {
        if (text !== undefined) {
            this.getMdastContent().value = text;
        }
        return this.getMdastContent().value;
    }
    language(language) {
        if (language !== undefined) {
            this.getMdastContent().lang = language;
        }
        return this.getMdastContent().lang;
    }
    eval() {
        if (this.language() === 'js') {
            return eval(this.text());
        }
        else {
            console.log(`cannot eval non-js language '${this.language()}'`);
        }
    }
}
export default Code;
