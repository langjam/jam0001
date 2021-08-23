const util = require("util");
const prettier = require("prettier");
const fs = require("fs");

const fnil = (x, d) => x || d;

const sourcePos = ({ source, line, column }) =>
    `(${source || "input"} ${line}:${column})`;

const Tok = Object.freeze({
    NUM: Symbol("TokNum"),
    ID: Symbol("TokId"),
    STRING: Symbol("TokString"),
    LET: Symbol("TokLet"),
    DO: Symbol("TokDo"),
    IF: Symbol("TokIf"),
    THEN: Symbol("TokThen"),
    ELSE: Symbol("TokElse"),
    FN: Symbol("TokFn"),
    DEF: Symbol("TokDef"),
    LEFT: Symbol("TokLeft"),
    RIGHT: Symbol("TokRight"),
    OPERATOR: Symbol("TokOpDef"),
    CMT: Symbol("TokComment"),
    LCMT: Symbol("TokLComment"),
    LPAR: Symbol("TokLParen"),
    RPAR: Symbol("TokRParen"),
    LCURL: Symbol("TokLCurl"),
    RCURL: Symbol("TokRCurl"),
    OP: Symbol("TokOp"),
    COMMA: Symbol("TokComma"),
    ARROW: Symbol("TokArrow"),
    EQUALS: Symbol("TokEquals"),
    SEMICOLON: Symbol("TokSemicolon"),
    EOF: Symbol("TokEOF"),
    WS: Symbol("TokWS"),
});

const lexerRules = {
    [Tok.WS]: {
        match: /^\s+/,
        ignore: true,
        transit: (state, match) => {
            let newlines;
            if ((newlines = match.match(/\n/g))) {
                state.ws = true;
                state.line += newlines.length;
                state.column = match.length - match.lastIndexOf("\n");
            }
        },
    },
    [Tok.NUM]: {
        match: /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/,
        vmap: parseFloat,
    },
    [Tok.ID]: {
        match: /^[A-Za-z]+[A-Za-z0-9_?!'-]*/,
        classify: {
            let: Tok.LET,
            do: Tok.DO,
            if: Tok.IF,
            then: Tok.THEN,
            else: Tok.ELSE,
            fn: Tok.FN,
            def: Tok.DEF,
            op: Tok.OPERATOR,
            left: Tok.LEFT,
            right: Tok.RIGHT,
        },
    },
    [Tok.CMT]: {
        match: /^\/\*/,
        until: /^\*\//,
        vmap: (c) => c.trim(),
    },
    [Tok.LCMT]: {
        match: /^\/\/.*$/m,
        vmap: (c) => c.trim(),
    },
    [Tok.LPAR]: {
        match: /^\(/,
    },
    [Tok.RPAR]: {
        match: /^\)/,
    },
    [Tok.LCURL]: {
        match: /^\{/,
    },
    [Tok.RCURL]: {
        match: /^\}/,
    },
    [Tok.COMMA]: {
        match: /^,/,
    },
    [Tok.SEMICOLON]: {
        match: /^\;/,
    },
    [Tok.STRING]: {
        match: /^"(?:\\"|[^"])+"/,
        vmap: (s) => JSON.parse(s),
    },
    [Tok.OP]: {
        match: /^[`~!@#$%^&*_+=:<>./?-]+/,
        classify: {
            "=": Tok.EQUALS,
            "=>": Tok.ARROW,
        },
    },
};

const lexer = (rules) => (input) => {
    function* genfunc(input) {
        let offset = 0;
        let state = { ws: false, line: 0, column: 0 };
        loop: while (input.length > 0) {
            for (let r of Object.getOwnPropertySymbols(rules)) {
                const { match, vmap, ignore, until, classify, transit } =
                    rules[r];
                let [m] = fnil(input.match(match), []);
                if (m) {
                    if (until) {
                        let level = 1;
                        input = input.slice(m.length);
                        let ret = m;
                        while (level > 0 || input.length <= 0) {
                            m = (input.match(match) || [])[0];
                            if (m) {
                                input = input.slice(m.length);
                                ret += m;
                                level++;
                            }
                            m = (input.match(until) || [])[0];
                            if (m) {
                                input = input.slice(m.length);
                                ret += m;
                                level--;
                                if (level == 0) {
                                    break;
                                }
                            }
                            ret += input[0];
                            input = input.slice(1);
                        }
                        if (level != 0) {
                            return new Error(
                                `Unclosed comment starting at ${sourcePos({
                                    offset,
                                    column: state.column,
                                    line: state.line,
                                })}: ${input.substr(0, 20)}...`
                            );
                        }
                        yield {
                            kind: r,
                            value: (vmap || ((x) => x))(ret),
                            pos: {
                                offset,
                                precNL: state.ws,
                                column: state.column,
                                line: state.line,
                            },
                        };
                        offset += ret.length;
                        if (transit) {
                            transit(state, m);
                        } else {
                            state.column += m.length;
                            state.ws = false;
                        }
                        continue loop;
                    }
                    if (!ignore)
                        yield {
                            kind: classify ? classify[m] || r : r,
                            value: (vmap || ((x) => x))(m),
                            pos: {
                                offset,
                                precNL: state.ws,
                                column: state.column,
                                line: state.line,
                            },
                        };
                    input = input.slice(m.length);
                    offset += m.length;
                    if (transit) {
                        transit(state, m);
                    } else {
                        state.column += m.length;
                        state.ws = false;
                    }
                    continue loop;
                }
            }
            return new Error(
                `Unexpected input at ${sourcePos({
                    offset,
                    column,
                    line,
                })}: ☞${input.substr(0, 20)}...`
            );
        }
        return {
            kind: Tok.EOF,
            pos: {
                offset,
                column: state.column,
                line: state.line,
                precNL: state.ws,
            },
        };
    }

    const gen = genfunc(input);
    return {
        input,
        gen,
        cur: gen.next(),
        peek() {
            return this.cur.value;
        },
        report(n = 40) {
            const { offset } = this.peek().pos;
            let ret = "",
                start = offset,
                end = offset;
            for (let i = 0; i < n; i++) {
                if (offset + i >= this.input.length) break;
                const c = this.input[offset + i];
                if (c == "\n") break;
                end++;
            }
            for (let i = 1; i < n; i++) {
                if (offset - i < 0) break;

                const c = this.input[offset - i];
                if (c == "\n") break;
                start--;
            }
            ret = "┃ " + this.input.substr(start, end - start) + "\n┃ ";
            for (let i = 0; i < offset - start; i++) ret += " ";
            ret += "☝︎";
            return ret;
        },
        accept(kind) {
            if (this.peek().kind == kind) {
                this.next();
                return this.peek();
            } else
                throw new Error(
                    `SyntaxError: Expected ${kind.toString()} at ${sourcePos(
                        this.peek().pos
                    )} but got:\n${this.report()} `
                );
        },
        next() {
            this.cur = this.gen.next();
            //console.log(this.cur);
            if (this.cur.error) throw this.cur.error;
        },
    };
};

const Assoc = Object.freeze({
    LEFT: Symbol("Assoc.LEFT"),
    RIGHT: Symbol("Assoc.RIGHT"),
});

const ops = {
    $: { prec: 1, assoc: Assoc.RIGHT },
    "+": { prec: 20, assoc: Assoc.LEFT, js: true },
    "-": { prec: 20, assoc: Assoc.LEFT, js: true },
    "*": { prec: 30, assoc: Assoc.LEFT, js: true },
    "/": { prec: 30, assoc: Assoc.LEFT, js: true },
    "^": { prec: 40, assoc: Assoc.RIGHT },
    "==": { prec: 15, assoc: Assoc.LEFT, js: true },
    "!=": { prec: 15, assoc: Assoc.LEFT, js: true },
    ">": { prec: 16, assoc: Assoc.LEFT, js: true },
    "<": { prec: 16, assoc: Assoc.LEFT, js: true },
    ">=": { prec: 16, assoc: Assoc.LEFT, js: true },
    "<=": { prec: 16, assoc: Assoc.LEFT, js: true },
    APPLY: { prec: 1000, assoc: Assoc.LEFT },
    ".": { prec: 2000, assoc: Assoc.LEFT, js: true },
};

const isTerminator = (t) => {
    return (
        t.kind == Tok.RPAR ||
        t.kind == Tok.RCURL ||
        t.kind == Tok.SEMICOLON ||
        t.kind == Tok.THEN ||
        t.kind == Tok.ELSE ||
        t.kind == Tok.EOF
    );
};

const astComments = (ast, c) => {
    if (!c || c.length == 0) return ast;
    if (!ast.comments) ast.comments = [];
    if (Array.isArray(c)) ast.comments = ast.comments.concat(c);
    else ast.comments.push(c);
    return ast;
};

const eatComments = (ctx, all) => {
    const { tok } = ctx;
    const comments = [];
    while (true) {
        if (tok.peek().kind == Tok.CMT || tok.peek().kind == Tok.LCMT) {
            if (!all && tok.peek().pos.precNL) break;
            comments.push(tok.peek().value);
            tok.accept(tok.peek().kind);
        } else break;
    }
    return comments;
};

const parseExpr = (ctx, minPrec) => {
    const { optable, tok } = ctx;
    let ret = parseAtom(ctx); // eats comments
    while (true) {
        const cur = tok.peek();
        let op = null;
        if (cur.kind == Tok.OP) {
            if (optable[cur.value].prec < minPrec) break;
            op = cur.value;
            tok.accept(Tok.OP);
        } else if (isTerminator(cur)) {
            break;
        } else {
            op = "APPLY";
            if (optable[op].prec < minPrec) break;
        }

        let comments = eatComments(ctx);

        const { prec, assoc } = optable[op];
        let rhs = parseExpr(ctx, assoc == Assoc.LEFT ? prec + 1 : prec);
        ret = {
            t: "ap",
            op,
            lhs: ret,
            rhs: rhs,
        };
        if (comments.length > 0) astComments(ret, comments);
    }
    return ret;
};

const parseDo = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "do", exprs: [] };
    tok.accept(Tok.DO);
    let postDoComments = eatComments(ctx);
    tok.accept(Tok.LCURL);
    let postOpenComments = eatComments(ctx);
    let exprsComments = [];
    while (tok.peek().kind != Tok.RCURL) {
        let preExp = eatComments(ctx);
        exprsComments = exprsComments.concat(preExp);
        let expr = parseExpr(ctx, 1);
        if (expr.comments) exprsComments = exprsComments.concat(expr.comments);
        astComments(expr, preExp);
        ret.exprs.push(expr);
        if (tok.peek().kind != Tok.RCURL) tok.accept(Tok.SEMICOLON);
    }
    let preCloseComments = eatComments(ctx);
    if (tok.peek().kind == Tok.RCURL) tok.accept(Tok.RCURL);
    else
        throw new Error(
            `SyntaxError: Expected \`}\` at ${sourcePos(
                tok.peek().pos
            )} to close \`}\` from ${sourcePos(start.pos)}`
        );
    let allComments = postDoComments
        .concat(postOpenComments)
        .concat(exprsComments)
        .concat(preCloseComments);
    astComments(ret, allComments);
    return ret;
};

const parseIf = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "if", cond: null, then: null, els: { t: "nil" } };
    tok.accept(Tok.IF);
    let postIfComments = eatComments(ctx);
    ret.cond = astComments(parseExpr(ctx, 1), postIfComments);
    tok.accept(Tok.THEN);
    let postThenComments = eatComments(ctx);
    ret.then = astComments(parseExpr(ctx, 1), postThenComments);
    let postElseComments = [];
    if (tok.peek().kind == Tok.ELSE) {
        tok.accept(Tok.ELSE);
        postElseComments = eatComments(ctx);
        ret.els = astComments(parseExpr(ctx, 1), postElseComments);
    }
    let allComments = postIfComments
        .concat(postThenComments)
        .concat(postElseComments);
    return ret;
};

const parseArg = (ctx) => {
    const { tok } = ctx;
    const cur = tok.peek();
    eatComments(ctx);
    switch (cur.kind) {
        case Tok.ID:
            tok.accept(Tok.ID);
            return { t: "id", value: cur.value };
        default:
            throw new Error(
                `SyntaxError: Expected formal argument at ${sourcePos(
                    tok.peek().pos
                )} but got this instead:\n${tok.report()}`
            );
    }
};

const parseEquation = (ctx) => {
    const { tok } = ctx;
    const ret = {
        t: "eqn",
        lhs: { t: "pattern", name: null, args: [] },
        rhs: null,
    };
    eatComments(ctx);
    if (tok.peek().kind != Tok.ID) {
        if (tok.peek().kind == Tok.LPAR) {
            tok.accept(Tok.LPAR);
            if (tok.peek().kind != Tok.OP)
                throw new Error(
                    `SyntaxError: Expected operator at ${sourcePos(
                        tok.peek().pos
                    )} but got this instead:\n${tok.report()}`
                );
            //console.log(tok.peek());
            ret.lhs.name = tok.peek().value;
            tok.accept(Tok.OP);
            tok.accept(Tok.RPAR);
        } else
            throw new Error(
                `SyntaxError: Expected identifier at ${sourcePos(
                    tok.peek().pos
                )} but got this instead:\n${tok.report()}`
            );
    } else {
        ret.lhs.name = tok.peek().value;
        tok.accept(Tok.ID);
    }
    eatComments(ctx);
    while (tok.peek().kind != Tok.EQUALS) {
        ret.lhs.args.push(parseArg(ctx));
    }

    tok.accept(Tok.EQUALS);
    let exprComments = eatComments(ctx);
    ret.rhs = astComments(parseExpr(ctx, 1), exprComments);
    return ret;
};

// this mutates the optable :shrug:
const parseOpDef = (ctx) => {
    const { optable, tok } = ctx;
    tok.accept(Tok.OPERATOR);
    eatComments(ctx);
    if (tok.peek().kind != Tok.OP)
        throw new Error(
            `SyntaxError: Expected operator at ${sourcePos(
                tok.peek().pos
            )} but got this instead:\n${tok.report()}`
        );

    const op = tok.peek().value;
    tok.accept(Tok.OP);
    eatComments(ctx);
    if (tok.peek().kind != Tok.NUM)
        throw new Error(
            `SyntaxError: Expected a number (operator precedence) at ${sourcePos(
                tok.peek().pos
            )} but got this instead:\n${tok.report()}`
        );

    const prec = tok.peek().value;
    if (prec < 1 || Math.floor(prec) != prec)
        throw new Error(
            `SyntaxError: Invalid operator precedence number at ${sourcePos(
                tok.peek().pos
            )}: ${prec}`
        );

    tok.accept(Tok.NUM);
    let assoc = null;
    eatComments(ctx);
    if (tok.peek().kind == Tok.LEFT) {
        assoc = Assoc.LEFT;
        tok.accept(Tok.LEFT);
    } else if (tok.peek().kind == Tok.RIGHT) {
        assoc = Assoc.RIGHT;
        tok.accept(Tok.RIGHT);
    } else
        throw new Error(
            `SyntaxError: Expected \`left\` or \`right\` at ${sourcePos(
                tok.peek().pos
            )} but got this:\n${tok.report()}`
        );
    eatComments(ctx);
    optable[op] = { prec, assoc };
    return { t: "void" }; // not sure if good idea
};

const parseDef = (ctx) => {
    const { tok } = ctx;
    tok.accept(Tok.DEF);
    return parseEquation(ctx);
};

const parseAtom = (ctx) => {
    const { tok } = ctx;
    const atom = parseAtomRaw(ctx);
    if (tok.peek().kind == Tok.CMT || tok.peek().kind == Tok.LCMT) {
        astComments(atom, tok.peek().value);
        tok.accept(tok.peek().kind);
    }
    return atom;
};

const parseAtomRaw = (ctx) => {
    const { optable, tok } = ctx;
    let ret = null;
    const cur = tok.peek();
    switch (cur.kind) {
        case Tok.NUM:
            tok.accept(Tok.NUM);
            return { t: "num", value: cur.value };
        case Tok.STRING:
            tok.accept(Tok.STRING);
            return { t: "string", value: cur.value };
        case Tok.ID:
            tok.accept(Tok.ID);
            return { t: "id", value: cur.value };
        case Tok.DO:
            return parseDo(ctx);
        case Tok.IF:
            return parseIf(ctx);
        case Tok.LPAR:
            let start = tok.accept(Tok.LPAR);
            if (tok.peek().kind == Tok.RPAR) {
                tok.accept(Tok.RPAR);
                return { t: "nil" };
            }
            if (tok.peek().kind == Tok.OP) {
                const op = tok.peek();
                tok.accept(Tok.OP);
                const opd = optable[op.value];
                if (tok.peek().kind == Tok.RPAR) {
                    tok.accept(Tok.RPAR);
                    return { t: "op", op: op.value };
                } else {
                    const rhs = parseExpr(
                        ctx,
                        opd.assoc == Assoc.LEFT ? opd.prec + 1 : opd.prec
                    );
                    tok.accept(Tok.RPAR);
                    return {
                        t: "ap",
                        op: "APPLY",
                        lhs: { t: "op", op: op.value, swap: true },
                        rhs,
                    };
                }
            }
            ret = parseExpr(ctx, 1);
            if (tok.peek().kind == Tok.RPAR) tok.accept(Tok.RPAR);
            else
                throw new Error(
                    `SyntaxError: Expected \`)\` at ${sourcePos(
                        tok.peek().pos
                    )} to close ``)`` from ${sourcePos(start.pos)}`
                );

            return ret;
        default:
            throw new Error(
                `SyntaxError: Expected either number, identifier, \`(\` or \`do\` at ${sourcePos(
                    cur.pos
                )} but got:\n${tok.report()}`
            );
    }
};

const parseForm = (ctx) => {
    const { tok } = ctx;
    let comments = eatComments(ctx, true);
    const cur = tok.peek();
    if (cur.kind == Tok.EOF) return { t: "void" };
    let ret = null;

    switch (cur.kind) {
        case Tok.OPERATOR:
            ret = parseOpDef(ctx);
            break;
        case Tok.DEF:
            ret = parseDef(ctx);
            break;
        default:
            ret = parseExpr(ctx, 1);
    }
    tok.accept(Tok.SEMICOLON);
    comments = comments.concat(eatComments(ctx));
    return astComments(ret, comments);
};

const parseProgram = (ctx) => {
    const { tok } = ctx;
    const ret = { t: "program", forms: [] };
    try {
        while (tok.peek().kind != Tok.EOF) {
            const r = parseForm(ctx);
            if (r.t != "void") ret.forms.push(r);
        }
        return ret;
    } catch (e) {
        console.log(e);
        return null;
    }
};

const debugAST = (ast, indent = 0) => {
    if (!ast) return "<failed parse>";
    let p = "";
    for (let i = 0; i < indent; i++) p += " ";
    p += ast.t + " " + ast.comments + ": ";
    switch (ast.t) {
        case "num":
        case "string":
        case "id":
            p += ast.value;
            break;
        case "ap":
            p += ast.op + "\n";
            p += debugAST(ast.lhs, indent + 1);
            p += debugAST(ast.rhs, indent + 1);
            break;
        case "op":
            p += ast.op + "\n";
            break;
        case "do":
            p += "\n";
            for (let e of ast.exprs) p += debugAST(e, indent + 1);
            break;
        case "program":
            p += "\n";
            for (let e of ast.forms) p += debugAST(e, indent + 1);
            break;
        case "eqn":
            p += `${ast.lhs.name}\n`;
            for (let e of ast.lhs.args) p += debugAST(e, indent + 1);
            p += debugAST(ast.rhs, indent + 1);
            break;
        case "if":
            p += "\n";
            p += debugAST(ast.cond, indent + 1);
            p += debugAST(ast.then, indent + 1);
            p += debugAST(ast.els, indent + 1);
            break;
        case "nil":
            p += "nil";
            break;
        case "void":
            p += "<void>";
            break;
        default:
            p += "??";
    }
    if (p[p.length - 1] != "\n") p += "\n";
    return p;
};

const munge = (s) => {
    return s.replace(/[`~!@#%^&*_+=:<>./?-]/g, (s) => {
        return {
            "`": "_BT_",
            "~": "_TD_",
            "!": "_EX_",
            "@": "_AT_",
            "#": "_HS_",
            "%": "_PC_",
            "^": "_CT_",
            "&": "_AA_",
            "*": "_ST_",
            "=": "_EQ_",
            ":": "_CO_",
            "<": "_LT_",
            ">": "_GT_",
            ".": "_DT_",
            "/": "_SL_",
            "?": "_QM_",
            "-": "_DH_",
            "+": "_PS_",
        }[s];
    });
};

const isJSOp = ({ optable }, s) => {
    if (optable[s]) return optable[s].js;
    else return false;
};

const freshVar = (ctx) => {
    if (!ctx.varc) ctx.varc = 0;
    ctx.varc++;
    return `__${ctx.varc}`;
};

const wrapComment = (ast, s) => {
    let ret = s;
    if (ast.comments) {
        for (let c of ast.comments) {
            ret = `RT.WC(${s})(${JSON.stringify(c)})`;
        }
    }
    return ret;
};

const compile = (ctx, ast) => {
    switch (ast.t) {
        case "program": {
            let ret = "";
            ret += "(()=>{\n";
            for (let i = 0; i < ast.forms.length; i++) {
                const f = ast.forms[i];
                if (i == ast.forms.length - 1)
                    ret += "return (" + compile(ctx, f) + ");\n";
                else ret += compile(ctx, f) + ";\n";
            }
            ret += "})();\n";
            return wrapComment(ast, ret);
        }
        case "do": {
            let ret = "";
            ret += "(()=>{\n";
            for (let i = 0; i < ast.exprs.length; i++) {
                const f = ast.exprs[i];

                if (i == ast.exprs.length - 1)
                    ret += "return (" + compile(ctx, f) + ");\n";
                else ret += compile(ctx, f) + ";\n";
            }
            ret += "})();\n";
            return wrapComment(ast, ret);
        }
        case "eqn": {
            let lhs = `const ${munge(ast.lhs.name)} =`;
            let ret = "";
            for (let a of ast.lhs.args) ret += ` (${compile(ast, a)}) => `;
            ret += compile(ctx, ast.rhs);
            return lhs + wrapComment(ast, ret);
        }
        case "if":
            return wrapComment(
                ast,
                `((${compile(ctx, ast.cond)})?(${compile(
                    ctx,
                    ast.then
                )}):(${compile(ctx, ast.els)}))`
            );
        case "ap":
            let op = ast.op;
            if (op == "APPLY" || op == "$")
                return wrapComment(
                    ast,
                    `RT.V(${compile(ctx, ast.lhs)})(${compile(ctx, ast.rhs)})`
                );

            if (isJSOp(ctx, op)) {
                if (op == ".") {
                    if (ast.rhs.t == "id")
                        return wrapComment(
                            ast,
                            `((${compile(ctx, ast.lhs)})${op}${compile(
                                ctx,
                                ast.rhs
                            )})`
                        );
                    return wrapComment(
                        ast,
                        `((${compile(ctx, ast.lhs)})[${compile(ctx, ast.rhs)}])`
                    );
                }
                return wrapComment(
                    ast,
                    `((${compile(ctx, ast.lhs)})${op}(${compile(
                        ctx,
                        ast.rhs
                    )}))`
                );
            }

            return wrapComment(
                ast,
                `RT.V(${munge(op)})(${compile(ctx, ast.lhs)})(${compile(
                    ctx,
                    ast.rhs
                )})`
            );
        case "op":
            if (isJSOp(ctx, ast.op)) {
                const lhs = freshVar(ctx);
                const rhs = freshVar(ctx);
                if (ast.swap) {
                    if (ast.op == ".")
                        return wrapComment(
                            ast,
                            `((${lhs}) => (${rhs}) => ${rhs}[${lhs}])`
                        );

                    return wrapComment(
                        ast,
                        `((${lhs}) => (${rhs}) => ${rhs} ${ast.op} ${lhs})`
                    );
                }
                return wrapComment(
                    ast,
                    `((${lhs}) => (${rhs}) => ${lhs} ${ast.op} ${rhs})`
                );
            }
            if (ast.swap) {
                const lhs = freshVar(ctx);
                const rhs = freshVar(ctx);
                return wrapComment(
                    ast,
                    `((${lhs}) => (${rhs}) => ${munge(
                        ast.op
                    )}.valueOf()(${rhs})(${lhs}))`
                );
            }
            return wrapComment(ast, `${munge(ast.op)}`);
        case "id":
            return wrapComment(ast, `${munge(ast.value)}`);
        case "num":
            return wrapComment(ast, `${ast.value}`);
        case "string":
            return wrapComment(ast, JSON.stringify(ast.value));
        case "nil":
            return wrapComment(ast, "null");
        default:
            throw new Error("compile error");
    }
};

class Cell {
    constructor(value) {
        this.value = value;
        this.cmts = [];
    }
    withComment(comment) {
        this.cmts.push(comment);
        return this;
    }
    valueOf() {
        return this.value;
    }
    comments() {
        return this.cmts;
    }
}

const RT = {
    C(v) {
        return new Cell(v);
    },
    WC(v) {
        return (c) =>
            v instanceof Cell ? v.withComment(c) : RT.C(v).withComment(c);
    },
    E(v) {
        return v instanceof Cell ? v.comments() : [];
    },
    V(v) {
        if (v instanceof Cell) return v.valueOf();
        else return v;
    },
    println(x) {
        console.log(x);
        return x;
    },
    map(f) {
        return (c) => c.map(f);
    },
    reduce(f) {
        return (c) => c.reduce(f);
    },
};

let inputFile = process.argv[2] || "examples/hello.js";
let input = fs.readFileSync(inputFile, { encoding: "utf-8" });

const tokens = lexer(lexerRules)(input);
const context = { optable: ops, tok: tokens };
const parsed = parseProgram(context, 1);
//console.log(util.inspect(parsed, { depth: Infinity, colors: true }));
//console.log("--input:\n", input);
//console.log("--parsed:\n", debugAST(parsed));
const compiled = prettier.format(compile(context, parsed), { parser: "babel" });
//console.log("--compiled:\n", compiled);
console.log(
    "\n\nreturned value:\n" +
        util.inspect(eval(compiled).valueOf(), {
            colors: true,
            depth: Infinity,
        })
);
//console.log("input file", inputFile);
