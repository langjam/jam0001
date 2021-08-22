import { readFileSync } from "fs"
import * as p from "./parserCombinator"

run(readFileSync(process.argv[2], "utf8"), process.argv.slice(3).join(" "))

function run(text: string, input: string){
  const indent = /^[ \t]+/m.exec(text)?.[0] ?? "  "
  const lines = groupLines(text.split("\n").filter(x => x.trim().length).map<[string, number]>(x => [
    x.trim(),
    /^\s*/.exec(x)?.[0].length ?? 0,
  ]))

  const parseExpression = _parseExpression()

  process.stdout.write(toString(executeFunc(
    -1,
    { kind: "func", args: ["$input"], name: "_main", body: lines.map(parseLine) },
    Object.create(null),
    Object.assign(Object.create(null), {
      toNumber: (_: unknown, [x]: unknown[]) =>
        +(x as never),
    }),
    [input],
  )))

  type Scope = Record<string, { value: unknown } | undefined>
  type FuncScope = Record<string, ((indentLevel: number, args: unknown[]) => unknown) | undefined>

  function executeFunc(
    indentLevel: number,
    func: Statement & {kind: "func"},
    scope: Scope,
    funcScope: FuncScope,
    params: unknown[],
  ){
    scope = Object.create(scope)
    funcScope = Object.create(funcScope)
    function populateFuncScope(statements: Statement[]){
      for(const statement of statements)
        if(statement.kind === "func")
          funcScope[statement.name] = (i, a) => executeFunc(i, statement, scope, funcScope, a)
        else if("body" in statement)
          populateFuncScope(statement.body)
    }
    populateFuncScope(func.body)
    for(const [i, arg] of func.args.entries())
      scope[arg] = { value: params[i] }
    let stack: [Statement[], number, boolean][] = [[func.body, 0, false]]
    main: while(stack.length) {
      let [statements, index, repeat] = stack.pop()!
      if(index >= statements.length) {
        if(repeat) stack.push([statements, 0, true])
        continue
      }
      const statement = statements[index]
      if(statement.kind === "break") {
        if(repeat) continue
        while(true) {
          if(!stack.length)
            throw new Error("Nothing to break from")
          if(stack.pop()![2])
            continue main
        }
      }
      if(statement.kind === "continue") {
        if(repeat) {
          stack.push([statements, 0, true])
          continue
        }
        while(true) {
          if(!stack.length)
            throw new Error("Nothing to continue")
          let top = stack.pop()!
          if(top[2]) {
            stack.push([top[0], 0, true])
            continue main
          }
        }
      }
      if(statement.kind === "if") {
        let endIndex = index + 1
        let others: (Statement & {kind: "else" | "elseif"})[] = []
        while(statements[endIndex].kind === "else" || statements[endIndex].kind === "elseif") {
          others.push(statements[endIndex] as never)
          if(statements[endIndex++].kind === "else")
            break
        }
        stack.push([statements, endIndex, repeat])
        if(isTruthy(executeExpression(statement.condition)))
          stack.push([statement.body, 0, false])
        else
          for(const other of others)
            if(other.kind === "else" || isTruthy(executeExpression(other.condition))) {
              stack.push([other.body, 0, false])
              continue main
            }
        continue
      }
      stack.push([statements, index + 1, repeat])
      if(statement.kind === "return")
        return executeExpression(statement.value)
      if(statement.kind === "repeat") {
        stack.push([statement.body, 0, true])
        continue
      }
      if(statement.kind === "else" || statement.kind === "elseif")
        throw new Error("Nothing to else")
      if(statement.kind === "func")
        continue
      if(statement.kind === "assignment")
        (scope[statement.name] ??= { value: null }).value = executeExpression(statement.value)
      if(statement.kind === "comment")
        console.error(
          indent.repeat(indentLevel + stack.length)
          + statement.text.map(x => x[0] === "$" ? dbg(scope[x]?.value ?? null) : x).join(""),
        )

    }
    return null

    function executeExpression(expression: Expression): unknown{
      if(expression.kind === "literal")
        return expression.value
      if(expression.kind === "variable")
        return scope[expression.name]?.value ?? null
      if(expression.kind === "call") {
        const func = funcScope[expression.name]
        if(!func) throw new Error("Undefined func " + expression.name)
        return func(indentLevel + stack.length, expression.args.map(executeExpression))
      }
      if(expression.kind === "unaryOp") {
        const arg = executeExpression(expression.arg)
        if(expression.op === "!")
          return !isTruthy(arg)
        else
          return -toNum(arg)
      }
      if(expression.kind === "binaryOp") {
        const a = () => executeExpression(expression.args[0])
        const b = () => executeExpression(expression.args[1])
        if(expression.op === "==")
          return eq(a(), b())
        if(expression.op === "!=")
          return !eq(a(), b())
        if(expression.op === ">")
          return toNum(a()) > toNum(b())
        if(expression.op === "<")
          return toNum(a()) < toNum(b())
        if(expression.op === "<=")
          return toNum(a()) <= toNum(b())
        if(expression.op === ">=")
          return toNum(a()) >= toNum(b())
        if(expression.op === "&&")
          return isTruthy(a()) && isTruthy(b())
        if(expression.op === "||")
          return isTruthy(a()) || isTruthy(b())
        if(expression.op === "%")
          return toNum(a()) % toNum(b())
        if(expression.op === "+")
          return a() as never + b() as never
        if(expression.op === "-")
          return toNum(a()) - toNum(b())
        if(expression.op === "*")
          return toNum(a()) * toNum(b())
        if(expression.op === "/")
          return toNum(a()) / toNum(b())
        const x: never = expression.op
        return x
      }
      const x: never = expression
      return x
    }
  }

  function eq(a: unknown, b: unknown){
    return a === b
  }

  function isTruthy(value: unknown){
    return !!value
  }

  function dbg(value: unknown): string{
    if(typeof value === "number" || value === null)
      return value + ""
    else if(typeof value === "string")
      return JSON.stringify(value)
    return "null"
  }

  function toString(value: unknown){
    return value + ""
  }

  function toNum(value: unknown){
    return +(value as never)
  }

  type Statement =
    | { kind: "comment", text: string[] }
    | { kind: "func", name: string, args: string[], body: Statement[] }
    | { kind: "repeat", body: Statement[] }
    | { kind: "return", value: Expression }
    | { kind: "continue" | "break" }
    | { kind: "if" | "elseif", condition: Expression, body: Statement[] }
    | { kind: "else", body: Statement[] }
    | { kind: "assignment", name: string, value: Expression }
  function parseLine(line: Line): Statement{
    const [text, children] = line
    const body = children.map(parseLine)
    let m
    if(text === "repeat:")
      return { kind: "repeat", body }
    if(text === "else:")
      return { kind: "else", body }
    if((m = text.match(/^(else )?if (.+):$/)))
      return { kind: m[1] ? "elseif" : "if", condition: parseExpression(m[2]), body }
    if((m = text.match(/^(\w+)\(\s*((?:\$\w+\s*,\s*)*(?:\$\w+\s*,?))?\s*\):$/)))
      return { kind: "func", name: m[1], args: m[2].split(/\s*,\s*/).filter(x => x), body }
    if(body.length)
      throw new Error("Unexpected indentation on line " + JSON.stringify(text))
    if((m = text.match(/^#\s*(.*)$/)))
      return { kind: "comment", text: m[1].match(/(?:(?!\$\w+).)+|\$\w+/g) ?? [] }
    if((m = text.match(/^return(?: (.+))?$/)))
      return { kind: "return", value: parseExpression(m[1] || "null") }
    if(text === "continue" || text === "break")
      return { kind: text }
    if((m = text.match(/^(\$\w+)\s*=\s*(.+)$/)))
      return { kind: "assignment", name: m[1], value: parseExpression(m[2]) }
    throw new Error("Invalid statement")
  }

  type Expression =
    | { kind: "literal", value: unknown }
    | { kind: "variable", name: string }
    | { kind: "unaryOp", op: "0-" | "!", arg: Expression }
    // eslint-disable-next-line max-len
    | { kind: "binaryOp", op: "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | ">" | "<" | ">=" | "<=", args: [Expression, Expression] }
    | { kind: "call", name: string, args: Expression[] }
  function _parseExpression(){
    const numberLiteral = p.map(p.regex(/\d+|\d+.\d*|\d*.\d+/), (x): Expression => (
      { kind: "literal", value: +x }
    ))
    const stringLiteral = p.map(p.regex(/(["'`])((?:[^\\\n]|\\.)*?)\1/), (x): Expression => (
      { kind: "literal", value: x[2].replace(/\\./g, x => x === "\\n" ? "\n" : x[1]) }
    ))
    const variable = p.map(p.regex(/\$\w+/), (x): Expression => (
      { kind: "variable", name: x[0] }
    ))
    const exprs = p.optional(p.map(p.concat(
      p.multiple(p.suffix(() => expr, p.string(","))),
      p.suffix(() => expr, p.optional(p.string(","), null)),
    ), x => [...x[0], x[1]]), [])
    const val: p.Parser<Expression> = p.ws(p.or(
      numberLiteral,
      stringLiteral,
      variable,
      p.map(p.or(p.string("true"), p.string("false")), x => ({ kind: "literal" as const, value: x === "true" })),
      p.map(p.string("null"), () => ({ kind: "literal" as const, value: null })),
      p.surround(p.string("("), () => expr, p.string(")")),
      p.map(p.concat(
        p.regex(/\w+/),
        p.surround(
          p.string("("),
          exprs,
          p.string(")"),
        ),
      ), (x): Expression => (
        { kind: "call", name: x[0][0], args: x[1] }
      )),
    ))
    const op = p.ws(p.or(...([
      "+", "-", "*", "/", "%", "&&", "||", "==", "!=", "!", "<=", ">=", ">", "<",
    ] as const).map(x => p.string(x))))
    const expr: p.Parser<Expression> = p.map(
      p.multiple(p.or(val, op)),
      input => {
        let outputQueue = []
        let opStack = []
        let op = false
        for(const token of input) {
          if(typeof token !== "string") {
            outputQueue.push(token)
            op = true
            continue
          }
          if(token === "-" && !op) {
            opStack.push("0-")
            continue
          }
          while(opStack.length && getPrecedence(opStack[opStack.length - 1]) >= getPrecedence(token))
            outputQueue.push(opStack.pop()!)
          opStack.push(token)
          op = false
        }
        while(opStack.length)
          outputQueue.push(opStack.pop()!)
        let stack: Expression[] = []
        for(const x of outputQueue) {
          if(typeof x !== "string") {
            stack.push(x)
            continue
          }
          if(x === "!" || x === "0-")
            stack.push({ kind: "unaryOp", op: x, arg: stack.pop()! })
          else
            stack.push({ kind: "binaryOp", op: x as never, args: [stack.pop()!, stack.pop()!].reverse() as never })
        }
        return stack.pop()!
        function getPrecedence(op: string): number{
          switch(op) {
            case "||":
              return 0
            case "&&":
              return 1
            case "==":
            case "!=":
              return 2
            case "+":
            case "-":
              return 3
            case "*":
            case "/":
            case "%":
              return 4
            case "!":
              return 5
            default:
              throw new Error("Invalid operator " + op)
          }
        }
      },
    )
    return (text: string) => {
      const x = p.parse(expr, text)
      if(!x) throw new Error("Invalid expression " + text)
      return x
    }
  }

  type Line = [string, Line[]]
  function groupLines(lines: [string, number][]): Line[]{
    let result: Line[] = []
    for(let i = 0; i < lines.length;) {
      let line = lines[i]
      let children = []
      let indentMin = lines[i][1]
      for(i++; i < lines.length && lines[i][1] > indentMin; i++)
        children.push(lines[i])
      result.push([line[0], groupLines(children)])
    }
    return result
  }
}

