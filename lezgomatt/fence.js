const KEYWORDS = [
    "nil",
    "true", "false",
    "and", "or", "not",
    "let",
    "if", "else",
    "func", "return",
];

const LINE_COMMENT_MARKERS = [
    "++", "--",
];

const KEYSYMBOLS = [
    "[+", "+]",
    "(", ")",
    "==", "!=",
    "<=", "<",
    ">=", ">",
    "+", "-",
    "*", "/",
    "=", ",",
    "{", "}",
];

function* getTokens(input, lineMap) {
    let offset = 0;

    tokenLoop:
    while (offset < input.length) {
        // Skip whitespace
        while (offset < input.length && input[offset].match(/\s/)) {
            offset++;
        }

        if (offset >= input.length) {
            break tokenLoop;
        }

        // Number
        if (input[offset].match(/[0-9]/)) {
            let start = offset++;
            while (offset < input.length && input[offset].match(/[0-9]/)) {
                offset++;
            }
            if (input[offset] === ".") {
                offset++;
                while (offset < input.length && input[offset].match(/[0-9]/)) {
                    offset++;
                }
            }
            let end = offset;

            let value = Number(input.slice(start, end));

            yield { start, end, type: "num", value };
            continue tokenLoop;
        }

        // String
        if (input[offset] === "\"") {
            let start = offset++;
            let value = "";
            while (offset < input.length) {
                if (input[offset] === "\"") {
                    offset++;
                    break;
                } else if (input[offset] === "\\") {
                    offset++;
                    if (input[offset] === "\\") {
                        offset++;
                        value += "\\";
                    } else if (input[offset] === "n") {
                        offset++;
                        value += "\n";
                    } else if (input[offset] === "\"") {
                        offset++;
                        value += "\"";
                    } else {
                        let { line, column } = findLineColumnNumbers(lineMap, offset);
                        throw new Error(`Unknown escape character '\\${input[offset]}' in string at L${line}:C${column}`);
                    }
                } else if (input[offset] === "\n") {
                    let { line, column } = findLineColumnNumbers(lineMap, offset);
                    throw new Error(`Unexpected newline in string at L${line}:C${column}`);
                } else {
                    value += input[offset++];
                }
            }
            let end = offset;

            yield { start, end, type: "str", value };
            continue tokenLoop;
        }

        // Keyword or identifier
        if (input[offset].match(/[A-Z_a-z]/)) {
            let start = offset++;
            while (offset < input.length && input[offset].match(/[0-9A-Z_a-z]/)) {
                offset++;
            }
            let end = offset;

            let value = input.slice(start, end);

            if (KEYWORDS.includes(value)) {
                yield { start, end, type: value };
                continue tokenLoop;
            } else {
                yield { start, end, type: "ident", value };
                continue tokenLoop;
            }
        }

        // Line comment
        for (let marker of LINE_COMMENT_MARKERS) {
            if (input.slice(offset).startsWith(marker)) {
                let start = offset;
                offset += marker.length;
                while (offset < input.length && input[offset] !== "\n") {
                    offset++;
                }
                let end = offset;

                let value = input.slice(start + marker.length, end).trim();

                yield { start, end, type: marker, value };
                continue tokenLoop;
            }
        }

        // Symbols
        for (let symbol of KEYSYMBOLS) {
            if (input.slice(offset).startsWith(symbol)) {
                let start = offset;
                offset += symbol.length;
                let end = offset;

                yield { start, end, type: symbol };
                continue tokenLoop;
            }
        }

        let { line, column } = findLineColumnNumbers(lineMap, offset);
        throw new Error(`Unexpected character: '${input[offset]}' at L${line}:C${column}`);
    }
}

function parse(input, lineMap) {
    let tokens = Array.from(getTokens(input, lineMap)).filter(t => t.type !== "--");
    let context = { tokens, index: 0, lineMap };

    let functions = [];

    while (true) {
        let f = parseFuncDef(context);
        if (f == null) {
            break;
        }
        functions.push(f);
    }

    consumeNextToken(context, "EOF");

    return { type: "program", functions };
}

function parseFuncDef(context) {
    if (!nextTokenIs(context, "func")) {
        return null;
    }

    let funcToken = consumeNextToken(context);
    let name = consumeNextToken(context, "ident");

    let parameters = [];
    consumeNextToken(context, "(");
    if (nextTokenIs(context, "ident")) {
        parameters.push(consumeNextToken(context));
        while (nextTokenIs(context, ",")) {
            consumeNextToken(context);
            parameters.push(consumeNextToken(context, "ident"));
        }
    }
    consumeNextToken(context, ")");

    let body = parseBlock(context);
    if (body == null) {
        consumeNextToken(context, "<BLOCK>");
    }

    return {
        type: "func",
        start: funcToken.start,
        end: body.end,
        name,
        parameters,
        body,
    }
}

function parseBlock(context) {
    if (!nextTokenIs(context, "{")) {
        return null;
    }

    let openToken = consumeNextToken(context);
    let statements = [];
    while (true) {
        let s = parseStatement(context);
        if (s == null) {
            break;
        }
        statements.push(s);
    }
    let closeToken = consumeNextToken(context, "}");

    return { type: "block", start: openToken.start, end: closeToken.end, statements };
}

function parseStatement(context) {
    return (
        parseLineComment(context)
        || parseFuncCall(context)
        || parseVarDeclaration(context)
        || parseAssignment(context)
        || parseIfStatement(context)
        || parseReturnStatement(context)
    );
}

function parseLineComment(context) {
    if (nextTokenIs(context, "++")) {
        return consumeNextToken(context);
    }

    return null;
}

function parseFuncCall(context) {
    if (!nextTokenIs(context, "ident") || !nextTokenIs(context, "(", 1)) {
        return null;
    }

    let func = consumeNextToken(context);

    let args = [];
    consumeNextToken(context, "(");
    let expr = parseExpression(context);
    if (expr != null) {
        args.push(expr);
        while (nextTokenIs(context, ",")) {
            consumeNextToken(context);
            let expr = parseExpression(context);
            if (expr == null) {
                consumeNextToken(context, "<EXPR>");
            }
            args.push(expr);
        }
    }
    let closeToken = consumeNextToken(context, ")");

    return { type: "call", start: func.start, end: closeToken.end, func, args };
}

function parseVarDeclaration(context) {
    if (!nextTokenIs(context, "let")) {
        return null;
    }

    let letToken = consumeNextToken(context);
    let variable = consumeNextToken(context, "ident");
    consumeNextToken(context, "=");
    let value = parseExpression(context);
    if (value == null) {
        consumeNextToken(context, "<EXPR>");
    }

    return { type: "let", start: letToken.start, end: value.end, variable, value };
}

function parseAssignment(context) {
    if (!nextTokenIs(context, "ident") || !nextTokenIs(context, "=", 1)) {
        return null;
    }

    let variable = consumeNextToken(context);
    consumeNextToken(context, "=");
    let value = parseExpression(context);
    if (value == null) {
        consumeNextToken(context, "<EXPR>");
    }

    return { type: "assign", start: variable.start, end: value.end, variable, value };
}

function parseIfStatement(context) {
    if (!nextTokenIs(context, "if")) {
        return null;
    }

    let blocks = [];

    let ifToken = consumeNextToken(context);

    let condition = parseExpression(context);
    if (condition == null) {
        consumeNextToken(context, "<EXPR>");
    }

    let consequence = parseBlock(context);
    if (consequence == null) {
        consumeNextToken(context, "<BLOCK>");
    }

    blocks.push({ condition, consequence });

    while (nextTokenIs(context, "else") && nextTokenIs(context, "if", 1)) {
        consumeNextToken(context);
        consumeNextToken(context);

        let condition = parseExpression(context);
        if (condition == null) {
            consumeNextToken(context, "<EXPR>");
        }

        let consequence = parseBlock(context);
        if (consequence == null) {
            consumeNextToken(context, "<BLOCK>");
        }

        blocks.push({ condition, consequence });
    }

    if (nextTokenIs(context, "else")) {
        consumeNextToken(context);

        let consequence = parseBlock(context);
        if (consequence == null) {
            consumeNextToken(context, "<BLOCK>");
        }

        blocks.push({ consequence });
    }

    return { type: "cond", start: ifToken.start, end: blocks[blocks.length - 1].end, blocks };
}

function parseReturnStatement(context) {
    if (!nextTokenIs(context, "return")) {
        return null;
    }

    let returnToken = consumeNextToken(context);
    let value = parseExpression(context);
    if (value == null) {
        consumeNextToken(context, "<EXPR>");
    }

    return { type: "return", start: returnToken.start, end: value.end, value };
}

const BINARY_OPERATORS = {
    "or": 0,
    "and": 1,
    "==": 2,
    "!=": 2,
    "<": 3,
    "<=": 3,
    ">": 3,
    ">=": 3,
    "+": 4,
    "-": 4,
    "*": 5,
    "/": 5,
}

function parseExpression(context, level = 0) {
    let expr = parseAtomicExpr(context);
    if (expr == null) {
        return null;
    }

    let opToken = peeknextToken(context);
    while (
        opToken.type in BINARY_OPERATORS
        && BINARY_OPERATORS[opToken.type] >= level
    ) {
        consumeNextToken(context);
        right = parseExpression(context, level+1);
        expr = { type: opToken.type, start: expr.start, end: right.end, left: expr, right };

        opToken = peeknextToken(context);
    }

    return expr;
}

function parseAtomicExpr(context) {
    let funcCall = parseFuncCall(context);
    if (funcCall != null) {
        return funcCall;
    }

    if (nextTokenIs(context, "not")) {
        let notToken = consumeNextToken(context);
        let expr = parseExpression(context);
        return { type: "not", start: notToken.start, end: expr.end, expr };
    }

    if (nextTokenIs(context, "-")) {
        let negToken = consumeNextToken(context);
        let expr = parseExpression(context);
        return { type: "neg", start: negToken.start, end: expr.end, expr };
    }

    if (nextTokenIs(context, "[+")) {
        let startToken = consumeNextToken(context);
        let expr = parseExpression(context);
        let endToken = consumeNextToken(context, "+]");
        return { type: "plus", start: startToken.start, end: endToken.end, expr };
    }

    if (nextTokenIs(context, "(")) {
        let startToken = consumeNextToken(context);
        let expr = parseExpression(context);
        let endToken = consumeNextToken(context, ")");
        return { type: "expr", start: startToken.start, end: endToken.end, expr };
    }

    if (nextTokenIs(context, "ident")) {
        return consumeNextToken(context);
    }

    let literal = parseLiteral(context);
    if (literal != null) {
        return literal;
    }

    return null;
}

function parseLiteral(context) {
    if (
        nextTokenIs(context, "nil")
        || nextTokenIs(context, "true")
        || nextTokenIs(context, "false")
        || nextTokenIs(context, "num")
        || nextTokenIs(context, "str")
    ) {
        return consumeNextToken(context);
    }

    return null;
}

function peeknextToken(context, offset = 0) {
    if (context.index + offset >= context.tokens.length) {
        let lastToken = context.tokens[context.tokens.length - 1];
        return { type: "EOF", start: lastToken.end, end: lastToken.end };
    }

    return context.tokens[context.index + offset];
}

function nextTokenIs(context, tokenType, offset = 0) {
    return peeknextToken(context, offset).type === tokenType;
}

function consumeNextToken(context, tokenType = null) {
    if (tokenType != null && !nextTokenIs(context, tokenType)) {
        let token = peeknextToken(context);
        let { line, column } = findLineColumnNumbers(context.lineMap, token.start);
        throw new Error(`Expected '${tokenType}', got '${token.type}' at L${line}:C${column}`)
    }

    return context.tokens[context.index++];
}

function makeLineMap(input) {
    let offsets = [0];

    for (let i = 0; i < input.length; i++) {
        if (input[i] === "\n") {
            offsets.push(i + 1);
        }
    }

    return offsets;
}

function findLineColumnNumbers(lineMap, offset) {
    for (let i = 0; i < lineMap.length; i++) {
        if (i + 1 === lineMap.length || offset < lineMap[i + 1]) {
            return {
                line: i + 1,
                column: offset - lineMap[i] + 1,
            }
        }
    }
}

// TODO: clean up!
let globalInput = null;
let globalLineMap = null;

const runtime = `/** begin runtime **/
let $counter = {};
function $plusLine(line, message) {
    $counter[line] = ($counter[line] || 0) + 1;
    console.log(\`L\${line}: \${message}; count = \${$counter[line]}\`);
}

function $plus(line, expression, result) {
    console.log(\`L\${line}: \${expression} => \${result}\`);
    return result;
}
/** end runtime **/`;

function generateCode(ast) {
    let funcs = ast.functions.map(generateFunction).join("\n\n");

    return [runtime, funcs, "main();"].join("\n\n");
}

function generateFunction(func) {
    let code = "function ";
    code += func.name.value;
    code += `(${func.parameters.map(p => p.value).join(', ')}) `;
    code += generateBlock(func.body);

    return code;
}

function generateBlock(block) {
    return ["{", block.statements.map(generateStatement).join("\n"), "}"].join("\n");
}

function generateStatement(statement) {
    switch (statement.type) {
    case "++":
        let { line } = findLineColumnNumbers(globalLineMap, statement.start);
        return `$plusLine(${line}, ${JSON.stringify(statement.value)});`;
    case "call":
        return generateCall(statement) + ";";
    case "let":
        return `let ${statement.variable.value} = ${generateExpression(statement.value)};`;
    case "assign":
        return `${statement.variable.value} = ${generateExpression(statement.value)};`;
    case "cond":
        return generateCondition(statement);
    case "return":
        return `return ${generateExpression(statement.value)};`;
    default:
        throw new Error(`Unknown statement type: ${statement.type}`);
    }
}

function generateCall(call) {
    let f = call.func.value;
    if (f === "print") {
        f = "console.log";
    }

    return `${f}(${call.args.map(generateExpression).join(", ")})`
}

function generateCondition(cond) {
    let first = cond.blocks[0];
    let code = `if (${generateExpression(first.condition)}) `
    code += generateBlock(first.consequence);

    let rest = cond.blocks.slice(1);
    for (let b of rest) {
        if (b.condition != null) {
            code += ` else if (${generateExpression(b.condition)}) `
            code += generateBlock(b.consequence);
        } else {
            code += ` else ${generateBlock(b.consequence)}`;
        }
    }

    return code;
}

function generateExpression(expression) {
    switch (expression.type) {
    case "or":
        return `(${generateExpression(expression.left)} || ${generateExpression(expression.right)})`;
    case "and":
        return `(${generateExpression(expression.left)} && ${generateExpression(expression.right)})`;
    case "==":
        return `(${generateExpression(expression.left)} === ${generateExpression(expression.right)})`;
    case "!=":
        return `(${generateExpression(expression.left)} !== ${generateExpression(expression.right)})`;
    case "<":
        return `(${generateExpression(expression.left)} < ${generateExpression(expression.right)})`;
    case "<=":
        return `(${generateExpression(expression.left)} <= ${generateExpression(expression.right)})`;
    case ">":
        return `(${generateExpression(expression.left)} > ${generateExpression(expression.right)})`;
    case ">=":
        return `(${generateExpression(expression.left)} >= ${generateExpression(expression.right)})`;
    case "+":
        return `(${generateExpression(expression.left)} + ${generateExpression(expression.right)})`;
    case "-":
        return `(${generateExpression(expression.left)} - ${generateExpression(expression.right)})`;
    case "*":
        return `(${generateExpression(expression.left)} * ${generateExpression(expression.right)})`;
    case "/":
        return `(${generateExpression(expression.left)} / ${generateExpression(expression.right)})`;
    case "call":
        return generateCall(expression);
    case "not":
        return `(!${generateExpression(expression.expr)})`;
    case "neg":
        return `(-${generateExpression(expression.expr)})`;
    case "plus":
        let { line } = findLineColumnNumbers(globalLineMap, expression.start);
        let code = JSON.stringify(globalInput.slice(expression.expr.start, expression.expr.end));
        return `$plus(${line}, ${code}, ${generateExpression(expression.expr)})`;
    case "expr":
        return `(${generateExpression(expression.expr)})`;
    case "ident":
        return expression.value;
    case "nil":
        return "null";
    case "true":
        return "true";
    case "false":
        return "false";
    case "num":
        return expression.value.toString();
    case "str":
        return JSON.stringify(expression.value);
    default:
        throw new Error(`Unknown expression type: ${expression.type}`);
    }
}

function main() {
    if (process.argv.length < 3) {
        return console.error("ERROR: No input");
    }

    try {
        const fs = require("fs");
        let input = globalInput = fs.readFileSync(process.argv[2], "utf8");
        let lineMap = globalLineMap = makeLineMap(input);
        let ast = parse(input, lineMap);
        let output = generateCode(ast);
        console.log(output);
    } catch (e) {
        console.error(e.message);
    }
}

main();
