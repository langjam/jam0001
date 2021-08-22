import { inspect } from "util"
import * as p from "./parserCombinator"

const sourceFile = `
foo($bar):
  # First, we double $bar
  $newBar = $bar
  repeat:
    if $newBar == $bar * 2:
      break
    $bar = $bar + 1
  $bar = $newBar
  return $bar

return foo($input)
`

function run(text: string){
  const indent = /^[ \t]+/m.exec(text)?.[0] ?? "  "
  const lines = groupLines(text.split("\n").filter(x => x.trim().length).map<[string, number]>(x => [
    x.trim(),
    /^\s*/.exec(x)?.[0].length ?? 0,
  ]))

  const parseExpression = _parseExpression()

  return lines.map(parseLine)

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
      return { kind: "func", name: m[1], args: m[2].split(/\s*,\s*/).filter(x => x).map(x => x.slice(1)), body }
    if(body.length)
      throw new Error("Unexpected indentation on line " + JSON.stringify(text))
    if((m = text.match(/^#\s*(.*)$/)))
      return { kind: "comment", text: m[1].match(/(?:(?!\$\w+).)+|\$\w+/g) ?? [] }
    if((m = text.match(/^return(?: (.+))?$/)))
      return { kind: "return", value: parseExpression(m[1] || "null") }
    if(text === "continue" || text === "break")
      return { kind: text }
    if((m = text.match(/^\$(\w+)\s*=\s*(.+)$/)))
      return { kind: "assignment", name: m[1], value: parseExpression(m[2]) }
    throw new Error("Invalid statement")
  }

  type Expression =
    | { kind: "literal", value: unknown }
    | { kind: "variable", name: string }
    | { kind: "unaryOp", op: "0-" | "!", arg: Expression }
    | { kind: "binaryOp", op: "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=", args: [Expression, Expression] }
    | { kind: "call", name: string, args: Expression[] }
  function _parseExpression(){
    const numberLiteral = p.map(p.regex(/\d+|\d+.\d*|\d*.\d+/), (x): Expression => (
      { kind: "literal", value: +x }
    ))
    const stringLiteral = p.map(p.regex(/(["'`])((?:[^\\\n]|\\.)+?)\1/), (x): Expression => (
      { kind: "literal", value: x[2].replace(/\\./g, x => x === "\\n" ? "\n" : x[1]) }
    ))
    const variable = p.map(p.regex(/\$(\w+)/), (x): Expression => (
      { kind: "variable", name: x[1] }
    ))
    const val = p.ws(p.or(
      numberLiteral,
      stringLiteral,
      variable,
      p.surround(p.string("("), () => expr, p.string(")")),
      p.map(p.concat(
        p.regex(/\w+/),
        p.surround(
          p.string("("),
          p.optional(p.concat(
            p.multiple(p.suffix(() => expr, p.string(","))),
            p.suffix(() => expr, p.optional(p.string(","), null)),
          ), null),
          p.string(")"),
        ),
      ), (x): Expression => (
        { kind: "call", name: x[0][0], args: x[1] ? [...x[1][0], x[1][1]] : [] }
      )),
    ))
    const op = p.ws(p.or(...(["+", "-", "*", "/", "%", "&&", "||", "==", "!=", "!"] as const).map(x => p.string(x))))
    const expr: p.Parser<Expression> = p.map(
      p.multiple(p.or(val, op)),
      input => {
        console.log(input)
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

console.log(inspect(run(sourceFile), { depth: null, colors: true }))

