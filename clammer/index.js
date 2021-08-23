// @ts-check
const {readFileSync} = require('fs');
const ohm = require('ohm-js');
const { argv } = require('process');
const prompt = require('prompt-sync')();

function buildGrammar() {
    return ohm.grammar(readFileSync('grammar.ohm', 'utf-8'));
}

/**
 * @param {string} filename 
 * @param {ohm.Grammar} grammar 
 */
function match(filename, grammar=null) {
    grammar = grammar || buildGrammar();
    const m = grammar.match(readFileSync(filename, 'utf-8'));
    if (m.failed()) console.error(m.message);
    return m;
}

function jsValueToClammerValue(value) {
    if (typeof value == 'number') return {type: 'number', value};
    else if (typeof value == 'string') return {type: 'string', value};
    else if (typeof value == 'boolean') return {type: 'boolean', value};
    else if (value instanceof Array) return {type: 'array', value: value.map(v => jsValueToClammerValue(v))};
    else {
        const result = {};
        for (const key in value) {
            result[key] = jsValueToClammerValue(value[key]);
        }
        return result;
    }
}

class ClammerFileInterpreter {
    constructor(filename, comments = null) {
        this.filename = filename;
        this.comments = comments;
        this.grammar = buildGrammar();
        this.semantics = this.grammar.createSemantics();

        /** @type {object} */
        this.fileScope = {
            print: {
                type: 'internal_function',
                args: ['value'],
                execute: (value) => console.log(value.value)
            },
            write: {
                type: 'internal_function',
                args: ['value'],
                execute: (value) => process.stdout.write(value.value.toString())
            },
            forEach: {
                type: 'internal_function',
                args: ['value'],
                execute: (arr, fn) => {
                    for (const el of arr.value) {
                        this.call(fn, [el]);
                    }
                }
            },
            push: {
                type: 'internal_function',
                args: ['arr', 'value'],
                execute: (arr, value) => {
                    (arr.value).push(value);
                }
            },

            len: {
                type: 'internal_function',
                args: ['arr'],
                execute: (arr) => jsValueToClammerValue(arr.value.length)
            },
    
            getBlock: {
                type: 'internal_function',
                args: ['exp'],
                execute: (interpretedResult) => {
                    return {
                        type: 'block',
                        value: jsValueToClammerValue(interpretedResult)
                    }
                }
            },
    
            parseFloat: {
                type: 'internal_function',
                args: ['str'],
                execute: (interpretedResult) => {
                    return jsValueToClammerValue(parseFloat(interpretedResult.value));
                }
            },
    
            import: {
                type: 'internal_function',
                args: ['thing'],
                execute: (interpretedResult) => {
                    const str = interpretedResult.value;
                    const [filename, resource] = str.split('//');
                    const module = new ClammerFileInterpreter(filename);
                    module.interpret();
                    if (!module.lookup(resource)) {
                        throw `Invalid resource ${resource} in ${filename}`;
                    }
                    for (const [key, val] of module.closureMap.entries()) {
                        this.closureMap.set(key, val);
                    }
                    return module.lookup(resource);
                }
            },
    
            toString: {
                type: 'internal_function',
                args: ['val'],
                execute: (interpretedResult) => {
                    return jsValueToClammerValue(interpretedResult.value.toString());
                }
            },
    
            input: {
                type: 'internal_function',
                args: ['msg'],
                execute: (msg) => {
                    return jsValueToClammerValue(prompt(msg?.value || ''));
                }
            }
        };

        this.activeScope = this.fileScope;
        // Map of scope -> the scope's closure
        this.closureMap = new Map();
        this.closureMap.set(this.fileScope, null);

        this.init();
    }

    getClosure(scope) {
        return this.closureMap.get(scope);
    }

    /**
     * @param {string} ident
     */
    lookup(ident) {
        for (let scope = this.activeScope; scope; scope = this.getClosure(scope)) {
            if (ident in scope) return scope[ident];
        }
        throw `Unable to find ${ident}`
    }

    call(f, argValues) {
        if (f.type == 'internal_function') {
            return f.execute(...argValues);
        } else {
            const scope = {};
            for (let i = 0; i < f.args.length; i++) {
                scope[f.args[i]] = argValues[i];
            }
            f.module.closureMap.set(scope, f.scope);
            const beforeScope = f.module.activeScope;
            f.module.activeScope = scope;
            const result = f.body.interpret();
            f.module.activeScope = beforeScope;
            return result;
        }
    }

    init() {
        const _this = this;
        this.semantics.addAttribute('asBlock', {
            AssignmentExp_assignment(lhs, _, rhs) {
                return {
                    type: { type: 'string', value: 'assignment' },
                    lhs: { type: 'string', value: lhs.sourceString },
                    rhs: { type: 'string', value: rhs.sourceString },
                    sourceString: { type: 'string', value: this.sourceString },
                };
            },
        });
    
        this.semantics.addOperation('interpret', {
            Code: e => {
                let result = null;
                for (const exp of e.asIteration().children) {
                    result = exp.interpret();
                }
                return result;
            },
    
            CodeBlock_multiline: (_1, code, _2) => code.interpret(),
    
            CommentedExp(comments, exp) {
                const result = exp.interpret();
                // comments get executed after their block 😅
                for (const c of comments.asIteration().children) {
                    const ci = c.interpret();
                    if (ci?.type?.endsWith('function')) {
                        _this.call(ci, [{
                            type: 'block',
                            value: exp.asBlock
                        }]);
                    }
                }
                return result;
            },
            Comment(_, exp) {
                // during normal interpretation, we ignore the comments
                if (!_this.comments) return null;
    
                for (const c of _this.comments) {
                    // Hack
                    if (exp.sourceString.startsWith(c + '(')) {
                        return exp.interpret();
                    }
                }
    
                return null;
            },
            Exp: e => e.interpret(),
    
            DoWhile_simple(_1, doCode, _2, whilePred) {
                let result = null;
                do {
                    result = doCode.interpret();
                } while (whilePred.interpret().value)
                return result;
            },
    
            DoWhile_whilecase(_1, doCode, _2, whilePred, whileCode) {
                let result = null;
                do {
                    result = doCode.interpret();
                    if (whilePred.interpret().value) {
                        whileCode.interpret();
                    } else {
                        break;
                    }
                } while (true)
                return result;
            },
    
            DoWhile_elsecase(_1, doCode, _2, whilePred, whileCode, _3, elseCode) {
                let result = null;
                do {
                    result = doCode.interpret();
                    if (whilePred.interpret().value) {
                        whileCode.interpret();
                    } else {
                        elseCode.interpret();
                        break;
                    }
                } while (true)
                return result;
            },
    
            IfExp_if(_, pred, body) {
                return pred.interpret().value && body.interpret();
            },
            IfExp_ifelse(_1, ifPred, ifBody, elseIfs, _2, elseBody) {
                const predBodyPairs = [
                    [ifPred, ifBody],
                    ...(elseIfs.asIteration().children?.map(e => e.interpret()) || []),
                ]
                for (const [pred, body] of predBodyPairs) {
                    if (pred.interpret().value) {
                        return body.interpret();
                    }
                }
    
                return elseBody.interpret();
            },
    
            Array(_1, exps, _2) {
                return {
                    type: 'array',
                    sourceString: this.sourceString,
                    value: exps.asIteration().children.map(c => c.interpret())
                }
            },
            AssignmentExp: e => e.interpret(),
            AssignmentExp_assignment: (lhs, _, rhs) => _this.activeScope[lhs.sourceString] = rhs.interpret(),
            AddExp: e => e.interpret(),
            AddExp_plus(x, _, y) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: x.interpret().value + y.interpret().value,
                };
            },
            AddExp_minus(x, _, y) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: x.interpret().value - y.interpret().value,
                };
            },
    
            MulExp_times(x, _, y) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: x.interpret().value * y.interpret().value,
                };
            },
            MulExp_divide(x, _, y) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: x.interpret().value / y.interpret().value,
                };
            },
            MulExp_modulo(x, _, y) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: x.interpret().value % y.interpret().value,
                };
            },
    
            ComparisonExp_equal(a, _, b) {
                const lhs = a.interpret();
                const rhs = b.interpret();
                return {
                    type: 'boolean',
                    sourceString: this.sourceString,
                    lhs,
                    rhs,
                    value: lhs.value == rhs.value
                }
            },
            ComparisonExp_ge(a, _, b) {
                const lhs = a.interpret();
                const rhs = b.interpret();
                return {
                    type: 'boolean',
                    sourceString: this.sourceString,
                    lhs,
                    rhs,
                    value: lhs.value >= rhs.value
                }
            },
            ComparisonExp_gt(a, _, b) {
                const lhs = a.interpret();
                const rhs = b.interpret();
                return {
                    type: 'boolean',
                    sourceString: this.sourceString,
                    lhs,
                    rhs,
                    value: lhs.value > rhs.value
                }
            },
    
            ComparisonExp_lt(a, _, b) {
                const lhs = a.interpret();
                const rhs = b.interpret();
                return {
                    type: 'boolean',
                    sourceString: this.sourceString,
                    lhs,
                    rhs,
                    value: lhs.value < rhs.value
                }
            },
            ComparisonExp_le(a, _, b) {
                const lhs = a.interpret();
                const rhs = b.interpret();
                return {
                    type: 'boolean',
                    sourceString: this.sourceString,
                    lhs,
                    rhs,
                    value: lhs.value <= rhs.value
                }
            },
    
            FunctionExpression(_1, _2, args, _3, _4, code, _5) {
                return {
                    type: 'function',
                    sourceString: this.sourceString,
                    args: args.interpret(),
                    body: code,
                    module: _this,
                    scope: _this.activeScope,
                }
            },
    
            ObjectExpression(_1, entries, _2) {
                return {
                    type: 'object',
                    sourceString: this.sourceString,
                    value: Object.fromEntries(entries.asIteration().children.map(e => e.interpret()))
                };
            },
    
            ObjectEntries(key, _, val) {
                return [
                    key.sourceString,
                    val.interpret()
                ];
            },
    
            ObjectField(obj, _, field) {
                return obj.interpret().value[field.sourceString];
            },
    
            FunctionCall(fn, _1, args, _2) {
                return _this.call(fn.interpret(), args.asIteration().children.map(a => a.interpret()));
            },
    
            ExpExp_power(base, _, exp) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: Math.pow(base.interpret().value, exp.interpret().value)
                };
            },
    
            Args: e => e.asIteration().children.map(e => e.sourceString),
    
            ident(_1, _2) { return _this.lookup(this.sourceString); },
    
            numericLiteral(_) {
                return {
                    type: 'numeric',
                    sourceString: this.sourceString,
                    value: parseFloat(this.sourceString)
                }
            },
            boolLiteral: s => {
                return {
                    type: 'boolean',
                    sourceString: s.sourceString,
                    value: s.sourceString == 'true'
                }
            },
    
            stringLiteral(_1, _2, _3) {
                return {
                    type: 'string',
                    sourceString: this.sourceString,
                    value: this.sourceString.slice(1, -1),
                };
            }
        });
    }

    interpret() {
        const m = match(this.filename, this.grammar);
        return this.semantics(m).interpret();
    }
}

function interpret(filename, comments=null) {
    return new ClammerFileInterpreter(filename, comments).interpret();
}

function static_run(static_method, filename) {
    return interpret(filename, [static_method])
}

if (argv[2] == 'buildGrammar') console.log(buildGrammar());
else if (argv[2] == 'match') console.log(match(argv[3]));
else if (argv[2] == 'run') console.log(interpret(argv[3]));
else if (argv[2].startsWith('static:')) console.log(static_run(argv[2].split(':')[1], argv[3]));
else throw "Unexpected command"
