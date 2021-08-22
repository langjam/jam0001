import { inspect } from "util"

const sourceFile = `
foo($bar):
  $bar = $bar * 2
  repeat:
    break
  return $bar

return foo($input)
`

interface Line {
	text: string,
  indentation: number,
	children: Line[],
}

function run(text: string){
  const indent = /^[ \t]+/m.exec(text)?.[0] ?? "  "
  const lines = groupLines(text.split("\n").filter(x => x.trim().length).map<[string, number]>(x => [
    x.trim(),
    /^\s*/.exec(x)?.[0].length ?? 0,
  ]))

  return lines.map(parseLine)

  type Statement =
    | { kind: "comment", text: string }
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
      return { kind: "comment", text: m[1] }
    if((m = text.match(/^return(?: (.+))?$/)))
      return { kind: "return", value: parseExpression(m[1] || "null") }
    if(text === "continue" || text === "break")
      return { kind: text }
    if((m = text.match(/^\$(\w+)\s*=\s*(.+)$/)))
      return { kind: "assignment", name: m[1], value: parseExpression(m[2]) }
    throw new Error("Invalid statement")
  }

  type Expression = string
  function parseExpression(text: string): Expression{
    return text
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

