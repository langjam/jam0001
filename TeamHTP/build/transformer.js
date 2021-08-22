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
const transformer = {
    repl: (exprs) => exprs.join(';\n'),
    method: ([name, ...rest]) => `function ${name} {\n${rest.join(';\n')}\n}`,
    binary_message: ([op, param]) => `${op}(${param})`,
    keyword_message: sortKeypairs,
    message_pair: ([key, value]) => [key.slice(0, -1), value],
    unary_message: ([name]) => `${name}()`,
    exprs: (exprs) => exprs.join(';\n'),
    answer: ([v]) => `return ${v}`,
    assign: ([name, value]) => `${name} = ${value}`,
    keyword: ([callee, call, cascades]) => {
        const calls = [`${callee}.${call}`];
        for (const cascade of cascades) {
            calls.push(`${callee}.${cascade}`);
        }
        return `(${calls.join(', ')})`;
    },
    keypairs: sortKeypairs,
    keypair: ([key, value]) => [key.slice(0, -1), value],
    cascades: (cascades) => cascades,
    binary: ([left, op, right]) => `${left}.${op}(${right})`,
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
        return `function (${params}) {\n${body}\n}`;
    },
    temps: (words) => `let ${words.join(', ')}`,
    map: (entries) => `{ ${entries.join(', ')} }`,
    mappair: ([keyword, value]) => `${keyword} ${value}`,
    scalar: ([value]) => `(${value})`,
    kw: ([{ value }]) => value,
    number: ([{ value }]) => `(${parseFloat(value)})`,
    op: ([{ value }]) => value.split('').map(c => '$' + opNames[c]).join(''),
    param: ([{ value }]) => value.substr(1),
    string: ([{ value }]) => value,
    symbol: ([{ value }]) => `Symbol('${value.substr(1)}')`,
    word: ([{ value }]) => value,
};
export default transformer;
