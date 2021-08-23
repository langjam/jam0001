# lang

Welcome to lang, a language in which all control flow is done in directives.

## How to use
Simply navigate to https://somebody1234.github.io/jam0001/onyx/lang.html and start playing around.  
Alternatively, clone the repo and open `onyx/lang.html` in a browser.

## What are directives?
These modify the behavior of the expressions they come after.  
The syntax for these is the directive name, followed by any parameters they take, wrapped in parentheses.  
For example, `print[1] (if true)` only executes `print[1]` if `true` is `true`.

## List of directives
In this list, the syntactic construct the directive is bound to is qualified by "the".  
For example, "the expression of the `if` directive" of `foo (if true)` refers to `foo`.

`(a <type>)`
`(an <type>)`
Specifies the type of the function parameter or expression.  
"Type"s are just functions returning `boolean`s when passed a value as input.  
If the function returns false, the program prints an error message and exits.

`(returns a <type>)`
`(returns an <type>)`
Same as above, but tests the return type of the function instead.

`(if <condition>)`
Only execute the expression if `<condition>` evaluates to `true`.

`(unless <condition>)`
Only execute the expression if `<condition>` evaluates to `false`.

`(while <condition>)`
Repeatedly execute the expression while `<condition>` evaluates to `true`.

`(until <condition>)`
Repeatedly execute the expression while `<condition>` evaluates to `false`.

`(return this)`
Exits the current function call, making the function call evaluate to the value of the expression.

`(call this <name>)`
Sets the value of the variable `<name>` in the current scope to the value of the expression.

`(should be <target>)`
`(should equal <target>)`
Compares the value of the expression to the value of `<target>` and logs whether they are equal.
Note that this does not stop execution on failure.

`(for example <example>)`
Does nothing. This is mainly for documentation purposes, however note that `<example>` must still be a valid expression.

`(note <note>)`
Alternative syntax for comments.

## Directive short-circuiting
By default, directives execute from left-to-right.
However, some directives short-circuit, preventing the execution of all directives following it.
Short-circuiting conditions are listed below.

`(a <type>)`
`(an <type>)`
`(returns a <type>)`
`(returns an <type>)`
Ideally would never short-circuit if the language was statically typed.
Would halt execution completely if these directives were working.
As they are now, these never short-circuit since they do not have any effect.

`(if <condition>)`
`(unless <condition>)`
Short-circuits if the expression is not executed.

`(while <condition>)`
`(until <condition>)`
Always short-circuits.

`(return this)`
Always short-circuits.

`(call this <name>)`
`(should be <target>)`
`(should equal <target>)`
Never short-circuits.

## Syntax

### Literals
#### Strings
Strings are delimited by `"`. Individual characters in a string are either anything but `"` and `\`, or `\` followed by any character.  
For example, `"foo"` represents the string `foo` and `"\"\\"` represents the string `"\`.

#### Integers
Integers are a sequence of (ASCII decimal) digits (`0123456789`).  
In this prototype they are arbitrary precision, however this is subject to change.

#### Decimals
Decimals are a sequence of digits (as above), followed by a `.` and another sequence of digits.  
For example, `0.0` is a valid decimal literal, however `0.`, `.0` and `.` are not.

#### Identifiers
Identifiers are a sequence of alphanumeric chars or `_` that do not start with a digit.  
For example, `foo` and `_0` are identifiers.

### Operators
For the most part, these are the familiar operators, with a few removed:  

- Unary plus (`+foo`) and unary minus (`-foo`) have been removed, to prevent ambiguity that may occur because of the lack of commas in function parameter lists -
for example, if these exist, the parameters for `foo[1 -2]` would be ambiguous between `1 - 2`, and `1` and `-2`.
As a workaround, it is recommended to define and use a function `neg[value] { 0 - value (return this) }` to negate a number.
- Parentheses (`(` and `)`) are not used for grouping, since they are used for comments.
As a workaround, it is recommend ed to define and use a function `identity[value] { value (return this) }` to group expressions.
- Bitwise operators (`&`, `|`, `^` and `~`) and exponentiation (`**`) are not included.

Below is a list of operators from highest binding power to lowest:
- `!` (this is a prefix operator)
- `*`, `/`, `%`
- `+`, `-`
- `&&`, `||` (these short-circuit)
- `==`, `!=`, `<`, `<=`, `>`, `>=`
- `&&`
- `||`

### Function calls
`name[arg1 arg2]`

### Function definitions
Just `function`, followed by its name, an argument list delimited by `[]`, and a block.
Note that the name is optional.
```js
function name[arg1 (an integer) arg2] (returns an integer) {
  value (return this)
}
```

### Comments
Comments can come after colons (`:`) in directives.
The body of comments are lexed as per usual, so not all text may work in a comment.
Note that comments can contain nested `()` - but `[]` do not get the same special handling.
This is subject to change as this is a prototype.
This is especially useful to annotate the type of an argument or the return type of a function,
or to explain why a directive was used.
The `note` directive or an empty directive can be used for standalone comments.  
For example, `(if foo: this is the condition)`, `(note that this is a comment)` and `(: todo: thing)`

## Features not yet designed/implemented
- a type system
- generics
- variadic functions
