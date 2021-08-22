const opNames = {
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
};
const sortKeypairs = (pairs) => {
    const sorted = [...pairs].sort(([a,], [b,]) => {
        if (a < b)
            return -1;
        if (a > b)
            return 1;
        return 0;
    });
    const name = sorted.map(([part,]) => part).join('$');
    const params = sorted.map(([, param]) => param).join(', ');
    return `${name}(${params})`;
};
function makeTransformer(context) {
    return {
        repl: (exprs) => exprs.join(';\n'),
        method: ([name, ...rest]) => ({
            name: name.split('(')[0],
            func: `function ${name} {\n${rest.join(';\n')}\n}`
        }),
        binary_message: ([op, param]) => `${op}(${param})`,
        keyword_message: sortKeypairs,
        message_pair: ([key, value]) => [key.slice(0, -1), value],
        unary_message: ([name]) => `${name}()`,
        exprs: (exprs) => exprs.join(';\n'),
        answer: ([v]) => `return ${v}`,
        assign: ([name, value]) => `${name} = ${value}`,
        keyword: ([callee, call, cascades]) => {
            const calls = [`${callee}.${call}`];
            if (cascades) {
                for (const cascade of cascades) {
                    calls.push(`${callee}.${cascade}`);
                }
            }
            if (calls.length > 1) {
                return `(${calls.join(', ')})`;
            }
            else {
                return calls[0];
            }
        },
        keypairs: sortKeypairs,
        keypair: ([key, value]) => [key.slice(0, -1), value],
        cascades: (cascades) => cascades,
        binary: ([left, op, right]) => {
            if (op[0] != '$') {
                return `((${left}) ${op} (${right}))`;
            }
            return `${left}.${op}(${right})`;
        },
        unary: ([left, op]) => `${left}.${op}()`,
        array: (array) => `[${array.join(', ')}]`,
        block: ([p, ...rest]) => {
            let params = '';
            let body = rest.join('\n');
            if (typeof p === 'string') {
                body = `${p}\n${body}`;
            }
            else {
                const { children } = p;
                params = children.join(', ');
            }
            return `(${params}) => {\n${body}\n}`;
        },
        temps: (words) => `let ${words.join(', ')}`,
        map: (entries) => `{ ${entries.join(', ')} }`,
        mappair: ([keyword, value]) => `${keyword} ${value}`,
        scalar: ([value]) => `(${value})`,
        kw: ([{ value }]) => value,
        number: ([{ value }]) => `(${parseFloat(value)})`,
        op: ([{ value }]) => ['**', '*', '/', '%', '+', '-', '<<', '>>', '>>>', '<', '<=', '>', '>=', '==', '!=', '===', '!===', '&', '^', '|', '&&', '||', '??',].includes(value) ? value : value.split('').map(c => '$' + opNames[c]).join(''),
        param: ([{ value }]) => value.substr(1),
        string: ([{ value }]) => value,
        symbol: ([{ value }]) => `Symbol('${value.substr(1)}')`,
        word: ([{ value }]) => value in context ? `this.${value}` : value,
    };
}
export { makeTransformer };
