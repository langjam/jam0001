# Academia

**Team:** Blue Blazin

## Introduction

Academia is a programming language designed primarily for reading rather than writing.

Comments are first-class citizens in academia-lang. You can pass comments anywhere a value could go.

However comments in academia are special. They are references to some computation. Generally these references are listed at the bottom.

This is best explained via an example:

```
let pi = /* [1] the mathematical constant PI */

/* [[1]] {{ 3.14159 }} */
```

Academics (as users of academia call themselves) generally begin writing well explained pseudocode using academia comments and once the approach has crystalized add references to implementation details.

## Usage

To run the academia interpreter, you'll need python 3.10 installed. You can download it from:
https://www.python.org/downloads/release/python-3100rc1/

Then to run a `.acad` file, use:

```
$ python3.10 main.py <filepath>
```

## Examples

Some classic examples can be found in `fib.acad`, `helloworld.acad`, and `fizzbuzz.acad`.

## Implementation

The implementation is a simple tree-walking interpreter.

The data types are:

- Number
- String
- Null
- Boolean
- Dictionary

## Grammar

The grammar of academia is heavily inspired by Lox with the addition of academia's special comments and a dictionary as the single container type.

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
                ( "else" statement )?
printStmt :: "print" expressionOrComment ";"
whileStmt :: "while" "(" expression ")" statement
returnStmt :: "return" expressionOrComment? ";"
block :: "{" Declaration\* "}"
reference :: // TODO

expressionOrComment :: assignment | comment

assignment :: IDENTIFIER "=" assignment
| logic_or

logic_or :: logic_and ( "or" logic_and )*
logic_and :: equality ( "and" equality )*
equality :: comparison ( ( "!=" | "==" ) comparison )*
comparison :: term ( ( ">" | ">=" | "<" | "<=" ) term )*
term :: factor ( ( "-" | "+" ) factor )*
factor :: unary ( ( "/" | "*" ) unary )*
unary :: ( "!" | "-" ) unary | call
call :: primary ( "(" arguments? ")" )*
primary :: "true" | "false" | "null" | NUMBER
         | STRING | IDENTIFIER
         | "(" expressionOrComment ")"
         | "{" dictItems "}"

dictItems :: (STRING | NUMBER) ":" expressionOrComment

function :: IDENTIFIER "(" parameters? ")" block
parameters :: IDENTIFIER ( "," IDENTIFIER )*
arguments :: expressionOrComment ( "," expressionOrComment )*

comment :: "/*" whitespace_ "[" NUMBER+ "]" text* "*/"

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
