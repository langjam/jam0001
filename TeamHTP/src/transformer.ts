import {Token, Tree} from "./mouthful";

const opNames: Record<string, string> = {
    "~": "tilde",
    "@": "at",
    "%": "percent",
    "&": "and",
    "*": "star",
    "-": "minus",
    "=": "equals",
    "+": "plus",
    "\\": "backslash",
    "|": "pipe",
    ",": "comma",
    "<": "less",
    ">": "greater",
    "/": "slash",
    "?": "question",
}

const sortKeypairs = (pairs: [[string, string]]) => {
    const sorted = [...pairs].sort(([a,], [b,]) => {
        if (a < b) return -1
        if (a > b) return 1
        return 0
    })
    const name = sorted.map(([part,]) => part).join('$')
    const params = sorted.map(([, param]) => param).join(', ')
    return `${name}(${params})`
}

function makeTransformer(context) {
    return {
        repl: (exprs: string[]) => exprs.join(';\n'),
        method: ([name, ...rest]: string[]) => ({
            name: name.split('(')[0],
            func: `function ${name} {\n${rest.join(';\n')}\n}`
        }),
        binary_message: ([op, param]: [string, string]) => `${op}(${param})`,
        keyword_message: sortKeypairs,
        message_pair: ([key, value]: [string, string]) => [key.slice(0, -1), value],
        unary_message: ([name]: [string]) => `${name}()`,
        exprs: (exprs: string[]) => exprs.join(';\n'),
        answer: ([v]: [string]) => `return ${v}`,
        assign: ([name, value]: [string, string]) => `${name} = ${value}`,
        keyword: ([callee, call, cascades]: [string, string, string[]]) => {
            const calls = [`${callee}.${call}`]
            if (cascades) {
                for (const cascade of cascades) {
                    calls.push(`${callee}.${cascade}`)
                }
            }
            if (calls.length > 1) {
                return `(${calls.join(', ')})`
            } else {
                return calls[0]
            }
        },
        keypairs: sortKeypairs,
        keypair: ([key, value]: [string, string]) => [key.slice(0, -1), value],
        cascades: (cascades: string[]) => cascades,
        binary: ([left, op, right]: [string, string, string]) => {
            if (op[0] != '$') {
                return `((${left}) ${op} (${right}))`
            }
            return `${left}.${op}(${right})`
        },
        unary: ([left, op]: [string, string]) => `${left}.${op}()`,
        array: (array: string[]) => `[${array.join(', ')}]`,
        block: ([p, ...rest]: [Tree | string, ...string[]]) => {
            let params = '';
            let body = rest.join('\n');
            if (typeof p === 'string') {
                body = `${p}\n${body}`
            } else {
                const {children} = p
                params = children.join(', ')
            }
            return `(${params}) => {\n${body}\n}`
        },
        temps: (words: string[]) => `let ${words.join(', ')}`,
        map: (entries: string[]) => `{ ${entries.join(', ')} }`,
        mappair: ([keyword, value]: [string, string]) => `${keyword} ${value}`,
        scalar: ([value]: [string]) => `(${value})`,
        kw: ([{value}]: [Token]) => value as string,
        number: ([{value}]: Token[]) => `(${parseFloat(value)})`,
        op: ([{value}]: [Token]) => ['**', '*', '/', '%', '+', '-', '<<', '>>', '>>>', '<', '<=', '>', '>=', '==', '!=', '===', '!===', '&', '^', '|', '&&', '||', '??',].includes(value) ? value as string : (value as string).split('').map(c => '$' + opNames[c]).join(''),
        param: ([{value}]: [Token]) => (value as string).substr(1),
        string: ([{value}]: [Token]) => value as string,
        symbol: ([{value}]: [Token]) => `Symbol('${value.substr(1)}')`,
        word: ([{value}]: [Token]) => value in context ? `this.${value}` : value,
    }
}

export {makeTransformer}
