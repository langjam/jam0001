# Fence

A programing language with comments supercharged for debugging.

My entry for [JT's Lang Jam #1](https://github.com/langjam/jam0001) (theme: "first-class comments").


## Instructions

* Requires Node.js
* Transpiles to JavaScript

To compile:
```
node fence.js sample.fence > out.js
```

To run:
```
node out.js
```


## Main idea: Plus comments

The idea of plus comments are basically "runtime comments"
that aid in tracing code (for debugging or education).

Like comments in other programming languages (called "minus comments" here),
plus comments do not affect the result of your program.

There are two forms of plus comments:
* `++ message` -- the "line comment", which prints out the line number, message, and a counter; NOTE: only allowed at the statement level
* `[* expression *]` -- the "block comment", which prints out the line number, expression and its result; NOTE: only allowed at the expression level


## Syntax

Please see `sample.fence`
