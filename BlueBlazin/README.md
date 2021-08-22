# Team Blue Blazin

## Grammar

```
Declaration :: FunctionDclr
| VarDclr
| Stmt

FunctionDclr :: "function" function
VarDclr :: "let" IDENTIFIER ( "=" expressionOrComment )? ";"

Stmt :: exprOrCommentStmt
| ifStmt
| printStmt
| whileStmt
| returnStmt
| block
| reference

exprOrCommentStmt :: expressionOrComment ";"
ifStmt :: "if" "(" expressionOrComment ")" statement
printStmt :: "print" expressionOrComment ";"
whileStmt :: "while" "(" expression ")" statement
returnStmt :: "return" expressionOrComment? ";"
block :: "{" Declaration\* "}"
reference :: // TODO

expressionOrComment :: assignment | comment

assignment :: IDENTIFIER "=" assignment
| logic_or

logic_or :: logic_and ( "or" logic_and )_
logic_and :: equality ( "and" equality )_
equality :: comparison ( ( "!=" | "==" ) comparison )_
comparison :: term ( ( ">" | ">=" | "<" | "<=" ) term )_
term :: factor ( ( "-" | "+" ) factor )_
factor :: unary ( ( "/" | "_" ) unary )_
unary :: ( "!" | "-" ) unary | call
call :: primary ( "(" arguments? ")" )_
primary :: "true" | "false" | "null" | NUMBER
         | STRING | IDENTIFIER
         | "(" expressionOrComment ")"
         | "{" dictItems "}"

dictItems :: (STRING | NUMBER) ":" expressionOrComment

function :: IDENTIFIER "(" parameters? ")" block
parameters :: IDENTIFIER ( "," IDENTIFIER )*
arguments :: expressionOrComment ( "," expressionOrComment )*

comment :: activeComment | passiveComment
activeComment :: "/*" whitespace_ "[" NUMBER+ "]" text* "*/"
passiveComment :: "//" text* (NEWLINE | EOF)

reference :: "/*" "[[" "{" expressionOrComment "}" "]]" "*/"

NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
STRING         → "\"" <any char except "\"">* "\"" ;
IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
DIGIT          → "0" ... "9" ;
```

## Example Program

Naive Fib

```
function fib(n) {
    if (n < 2) return 2;

    return /* [1]
        we recursively call fib on `n - 1` and `n - 2`, add the
        two up and return that result.
    */;
}

fib(5);

/* [[1]] {{ fib(n - 1) + fib(n - 2) }} */
```
