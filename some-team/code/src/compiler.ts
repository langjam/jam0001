import { readFileSync } from 'fs'
import ohm, { grammar } from 'ohm-js'

import { IBackend, Ref } from './backend/IBackend'
import { CPPBackend, cppemit } from './backend/CPPBackend'

if (process.argv.length != 4) {
    console.log('Usage: sucrase-node src/compiler.ts <input> <output>')
    process.exit(1)
}

const backend: IBackend = new CPPBackend()

const grammarText = readFileSync('src/grammar.ohm').toString()
const g = grammar(grammarText)

const codeFileName = process.argv[2]
const code = readFileSync(codeFileName).toString()

const matchResult = g.match(code)
if (matchResult.failed()) {
    console.log(matchResult.message)
    process.exit(1)
}

const globals = new Map<string, GlobalKind>()

enum GlobalKind {
    Function,
    Thread,
}

const sema = g.createSemantics()

sema.addOperation('noteglobals', {
    ThreadDecl(_atthread, _fn, name, _brackets, body) {
        globals.set(name.sourceString, GlobalKind.Thread)
    },
})
sema.addOperation('collectNodes', {
    NonemptyListOf(first, _sep, rest) {
        return [first, ...rest.children]
    },
})
sema.addOperation('consteval', {
    strlit(_q, body, _q2) {
        return body.sourceString
            .replace(/\\n/g, '\n')
            .replace(/\\"/g, '"')
            .replace(/\\\\/g, '\\')
    },
})
let variables = new Map<string, Ref>()
sema.addOperation('build', {
    ThreadDecl(_atthread, _fn, name, _brackets, body) {
        backend.declarethread(name.sourceString, () => {
            variables = new Map<string, Ref>()
            body.build()
        })
    },
    BlockStatement(_open, body, _close) {
        return body.build()
    },
    ExpressionStatement(expr, _sc) {
        return expr.build()
    },
    LetStatement(_let, ident, _eq, expr, _sc) {
        variables.set(
            ident.sourceString,
            backend.declarevar(ident.sourceString, expr.build())
        )
    },
    number(n) {
        return backend.numliteral(+n.sourceString)
    },
    InfiniteLoopStatement(_for, blockstmt) {
        return backend.declareloop(
            () => backend.boolliteral(true),
            () => {
                blockstmt.build()
            }
        )
    },
    AtLogExpression(_atlog, _obr, args, _cbr) {
        const cn: ohm.Node[] = args.collectNodes()
        const val = cn[0].consteval()
        if (typeof val != 'string') {
            console.error(`Unable to compile: @log expression invalid`)
            console.error(cn[0].source.getLineAndColumnMessage())
            process.exit(1)
        }
        if (cn.length != 1) {
            cn.shift()
            return backend.atlogsend(
                backend.interpolate(
                    val
                        .split('*')
                        .map((e, i) => {
                            if (i == 0) return [e]
                            return [cn.shift().build(), e]
                        })
                        .flat()
                )
            )
        }
        return backend.atlogsend(backend.strliteral(val))
    },
    AtPublishWorkExpression(_atpublish, _obr, work, _cbr) {
        return backend.pubwork(work.build())
    },
    Expression_variable(ident) {
        return variables.get(ident.sourceString) || backend.nilliteral()
    },
    RFCExpression(_atrfc, _open, work, _close) {
        return backend.rfc(work.build())
    },
    IfStatement(_if, cond, blockstmt, _else, elseblockstmt) {
        return backend.if(
            cond.build(),
            () => {
                blockstmt.build()
            },
            () => {
                if (elseblockstmt) {
                    elseblockstmt.build()
                }
            }
        )
    },
    ParenExpression(_obr, expr, _cbr) {
        return expr.build()
    },
    Expression_equality(left, _eq, right) {
        return backend.equal(left.build(), right.build())
    },
    Expression_atom(_br, atom) {
        return backend.symbol(atom.sourceString)
    },
    AssignStatement_add(tgd, _peq, expr, _sc) {
        backend.setvar(
            variables.get(tgd.sourceString),
            backend.add(variables.get(tgd.sourceString), expr.build())
        )
    },
    SeeWorkExpression(_seework, _obr, worker, _cbr) {
        return backend.seework(worker.consteval())
    },
    CommentExpression(_atcomment, _obr, work, _comma, comment, _cbr) {
        return backend.comment(work.build(), comment.build())
    },
    BreakStatement(_break, _sc) {
        backend.break();
    }
})

const semapplied = sema(matchResult)

semapplied.noteglobals()
semapplied.build()

cppemit()