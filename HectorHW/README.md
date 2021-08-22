# CondCom language

This small language implements what I call conditional 
comments which are basically comments that can be used as control flow.

## Build steps
The language is written in Rust and can be built using `cargo`. Simply run `cargo run --release examples/fact.txt`
(or pass any other path). The project uses `lalrpop` which itself depends on many other libs so build may take some time.

## Language basics
### statements and expressions
Typical program consists of statements terminated by semicolon. Each statement may be accompanied by one or more single-line 
comments that start with double slash `//`. There are print statements written like `print expr;`, 
assignments `variable = expr;`, expression statements `expr;` and gotos written like `goto some label;` (spaces are fine).

Expressions can be composed of string literals like `"abc"` with support of some escape sequences (strings can be multiline),
variables consisting of latin letters, positive numbers, binary arithmetic operators (`+`,`-`, `*`, `/`, `%`), unary `meh` 
operator that is used in a way similar to unary not but for numbers, function definitions and function calls.

The language supports 4 types of values: **numbers** that are signed 64-bit and can be used as logical values where 0 is treated as false, and 
any other number is considered to be true (for that to work multiplication is short-circuited; use `meh` if you need to invert a value);
**strings** are defined with string literals and can be concatenated using `+` with other strings and numbers. **Functions** come in two forms: 
user-defined with syntax `function (args separated by commas) (function body consisting of statements)` and native predefined functions (`read`, `readnum`, `isstring`, `isnum`).
In user-defined functions statement that is executed last provides return value. (result of expression for expression statement like `1;`, produced value in print statements and so on).
All functions are anonymous and variable lookup is limited by bounds of current function, but you may define recursive functions by calling the same function inside function's body with `self(args)`.
### comments
As mentioned before, Comments are used for control flow. Some comments take arguments which are just expressions surrounded with dollar signs `$` (like `//when $a>0$`)

| comment | description |
| --- | --- |
| `//when $expr$` | this comment can conditionally exclude statement from execution (something like classic if). Sets conditional variables.|
| `//otherwise when $expr$` | same as above, runs conditionally if previous test failed (elif)|
| `//otherwise` | runs if previous tests failed |
|`//conditionally`| resets conditional variables|
|`//and`| used with `when`, `otherwise`, `once` to affect multiple statements|
|`//never`| prevents any statement from executing|
|`//once`|runs any statement once (per function body or script), can be accompanied with `otherwise`|
|`//assert $expr$`|can be used to check some vital conditions, terminates program if fails|
|`//any other`| can be used as goto target or classic documenting comment|

## Examples
you may find examples inside `examples` directory.

|file| description|
| --- | --- |
| foobar.txt | prints foo/bar/foobar|
| functions.txt| example of defining functions, multiline strings|
| fact.txt | computes factorial of given number or informs user about malformed input|
| fib.txt| computes some fibonacci numbers|
|natives.txt| will tell you some facts about your future|

