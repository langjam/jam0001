/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import { createToken, CstParser, Lexer } from 'chevrotain'
import { debug, TODO } from './debug'
import { interpret, StackValue } from './interpreter'
import { CallOperation, DupeOperation, Operation, PopOperation, PushOperation, RollOperation, SwapOperation } from './operations'
import { procedures } from './procedures'

const WHITE_SPACE_T = createToken({name: "WhiteSpace", pattern: /\s+/, group: Lexer.SKIPPED })

const WHITE_SPACE_NO_NEW_LINE_T = createToken({name: "WhiteSpaceNoNewLine", pattern: /[ \t]+/, group: Lexer.SKIPPED })

const NEW_LINE_POP_T = createToken({name: "NewLinePop", pattern: /\r?\n/, pop_mode: true })

const NOT_NEW_LINE_T = createToken({name: "NotNewLine", pattern: /[^(?:\r?\n)]+/ })

const ARGUMENT_T = createToken({ name: "ArgumentBody", pattern: /\d+/ }) //TODO this only supports integers for now

const PROCEDURE_NAME_T = createToken({ name: "ProcedureName", pattern: /[a-zA-Z]+/ })

//Single Line Comment Tokens
const BASIC_COMMENT_T = createToken({ name: "BasicComment", pattern: /REM /, push_mode: "basic_mode" })
const C_COMMENT_T = createToken({name: "CComment", pattern: /\/\//, push_mode: "no_arg_mode"  })
const PERL_COMMENT_T = createToken({name: "PerlComment", pattern: /#/, push_mode: "perl_mode" })
const HASKELL_COMMENT_T = createToken({name: "HaskellComment", pattern: /--/, push_mode: "no_arg_mode" })
const ASSEMBLY_COMMENT_T = createToken({name: "AssemblyComment", pattern: /;/, push_mode: "no_arg_mode" })
const MATLAB_COMMENT_T = createToken({name: "MatlabComment", pattern: /%/, push_mode: "no_arg_mode" })

//TODO add multi-line comments

const allTokens = {
    modes: {
        outside_mode: [
            WHITE_SPACE_T,
            BASIC_COMMENT_T,
            C_COMMENT_T,
            PERL_COMMENT_T,
            HASKELL_COMMENT_T,
            ASSEMBLY_COMMENT_T,
            MATLAB_COMMENT_T,
        ],
        basic_mode: [
            WHITE_SPACE_NO_NEW_LINE_T,
            NEW_LINE_POP_T,
            ARGUMENT_T,
            NOT_NEW_LINE_T
        ],
        no_arg_mode: [
            WHITE_SPACE_NO_NEW_LINE_T,
            NEW_LINE_POP_T,
            NOT_NEW_LINE_T
        ],
        perl_mode: [
            WHITE_SPACE_NO_NEW_LINE_T,
            PROCEDURE_NAME_T,
            NOT_NEW_LINE_T,
            NEW_LINE_POP_T,
        ]
    },
    defaultMode: "outside_mode"
}

class COMPParser extends CstParser {
    constructor() {
        super(allTokens, {maxLookahead: 4}) //TODO this probably doesn't need to be 4

        const $ = this

        $.RULE('script', () => {
            $.MANY(() => {
                $.SUBRULE($.topLevel)
            })
        })

        $.RULE('topLevel', () => {
            $.OR([
                { ALT: () => $.SUBRULE($.singleLineComment) },
                //{ ALT: () => $.SUBRULE2($.multiLineComment) }
            ])
        })

        $.RULE('singleLineComment', () => {
            $.OR([
                { ALT: () => $.SUBRULE($.basicComment) },
                { ALT: () => $.SUBRULE($.haskellComment) },
                { ALT: () => $.SUBRULE($.perlComment) },
                { ALT: () => $.SUBRULE($.cComment) },
                { ALT: () => $.SUBRULE($.assemblyComment) },
                { ALT: () => $.SUBRULE($.matlabComment) }
            ])
        })

        $.RULE('basicComment', () => {
            $.CONSUME(BASIC_COMMENT_T)
            $.CONSUME(ARGUMENT_T)
            $.OPTION(() => { $.CONSUME(NOT_NEW_LINE_T) })
            $.OPTION2(() => { $.CONSUME(NEW_LINE_POP_T) })
        })

        $.RULE('haskellComment', () => {
            $.CONSUME(HASKELL_COMMENT_T)
            $.OPTION(() => { $.CONSUME(NOT_NEW_LINE_T) })
            $.OPTION2(() => { $.CONSUME(NEW_LINE_POP_T) })
        })

        $.RULE('perlComment', () => {
            $.CONSUME(PERL_COMMENT_T)
            $.CONSUME(PROCEDURE_NAME_T)
            $.MANY(() => $.OR([
                { ALT: () => $.CONSUME(NOT_NEW_LINE_T) },
                { ALT: () => $.CONSUME2(PROCEDURE_NAME_T) }
            ]))
            $.OPTION2(() => { $.CONSUME(NEW_LINE_POP_T) })
        })

        $.RULE('cComment', () => {
            $.CONSUME(C_COMMENT_T)
            $.OPTION(() => { $.CONSUME(NOT_NEW_LINE_T) })
            $.OPTION2(() => { $.CONSUME(NEW_LINE_POP_T) })
        })

        $.RULE('assemblyComment', () => {
            $.CONSUME(ASSEMBLY_COMMENT_T)
            $.OPTION(() => { $.CONSUME(NOT_NEW_LINE_T) })
            $.OPTION2(() => { $.CONSUME(NEW_LINE_POP_T) })
        })

        $.RULE('matlabComment', () => {
            $.CONSUME(MATLAB_COMMENT_T)
            $.OPTION(() => { $.CONSUME(NOT_NEW_LINE_T) })
            $.OPTION2(() => { $.CONSUME(NEW_LINE_POP_T) })
        })

        // $.RULE('multiLineComment', () => {
        //     //TODO
        // })

        this.performSelfAnalysis()
    }

    //properties below just exist to make TS happy
    script: any
    topLevel: any
    singleLineComment: any
    basicComment: any
    haskellComment: any
    perlComment: any
    cComment: any
    assemblyComment: any
    matlabComment: any
//    multiLineComment: any
}

const compLexer = new Lexer(allTokens)
const compParser = new COMPParser()
const BaseCOMPVisitor = compParser.getBaseCstVisitorConstructor()

/**
 * A visitor for COMP that focuses on converting Chevrotain's CTS to an internal AST.
 */
class COMPVisitor extends BaseCOMPVisitor {
    constructor() {
        super()
        this.validateVisitor()
    }

    script(ctx: any): Array<Operation> {
        //debug("script", ctx)
        let operations = Array<Operation>()
        if (ctx.topLevel != undefined) {
            for (let ts of ctx.topLevel) {
                operations.push(this.topLevel(ts.children))
            }
        }
        return operations
    }

    topLevel(ctx: any): Operation { //TODO will probably return an array eventually
        if (ctx.singleLineComment != undefined) {
            return this.visit(ctx.singleLineComment)
        } else if (ctx.multiLineComment != undefined) {
            return this.visit(ctx.multiLineComment)
        } else {
            throw new Error("Not implemented.")
        }
    }

    singleLineComment(ctx: any): any {
        if (ctx.basicComment != undefined) {
            return this.visit(ctx.basicComment)
        } else if (ctx.haskellComment != undefined) {
            return this.visit(ctx.haskellComment)
        } else if (ctx.perlComment != undefined) {
            return this.visit(ctx.perlComment)
        } else if (ctx.cComment != undefined) {
            return this.visit(ctx.cComment)  
        } else if (ctx.assemblyComment != undefined) {
            return this.visit(ctx.assemblyComment)
        } else if (ctx.matlabComment != undefined) {
            return this.visit(ctx.matlabComment)
        } else {
            throw debug("Not supported", ctx)
        }
    }

    basicComment(ctx: any): Operation {
        let value = Number(ctx.ArgumentBody[0].image)
        return new PushOperation(value)
    }

    haskellComment(ctx: any): Operation {
        return new PopOperation()
    }

    perlComment(ctx: any): Operation {
        let procedureName = ctx.ProcedureName[0].image
        return new CallOperation(procedureName)
    }

    cComment(ctx: any): Operation {
        return new DupeOperation()
    }

    assemblyComment(ctx: any): Operation {
        return new RollOperation()
    }

    matlabComment(ctx: any): Operation {
        return new SwapOperation()
    }

    // multiLineComment(ctx: any): any {
    //     return TODO()
    // }
}

export class COMPError { 
    readonly message: string 
    constructor(message: string) {
        this.message = message
    }
}

const compVisitor = new COMPVisitor()

export class COMPInterpreter {
    run(script: string, stack: Array<StackValue> = new Array()): Array<StackValue> | COMPError {
        const res = this.createOperations(script)
        if (res instanceof COMPError) {
            return res
        } else {
            return interpret(res, stack)
        }
    }

    createOperations(script: string): Array<Operation> | COMPError  {
        const lexResult = compLexer.tokenize(script);
        if (lexResult.errors.length > 0) {
            return new COMPError(`Lexing Error: ${lexResult.errors.toString()}`); //TODO make message better/multiple messages?
        }
        
        compParser.input = lexResult.tokens;
        let parseResult = compParser.script()
        if (compParser.errors.length > 0) {
            return new COMPError(`Parsing Error: ${compParser.errors}`) //TODO make message better/multiple messages?
        }

        const res = compVisitor.visit(parseResult);
        return res;
    }
}
