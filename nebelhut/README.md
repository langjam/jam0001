🦩 Flamingo
========

Getting started
--------

To build the interpreter you can call `build.sh` or just compile `flamingo.cpp` with your favorite C++ compiler. (You need to do that in this directory as `flamingo.cpp` includes some files.) You can call `flamingo` with a filename to execute the entire file; or without arguments to launch the REPL.

To print something to `stdout` you can use the builtin function `println`:

```
println "Hello, world."
```

An overview over the syntax and all functions is given at the end.


Values
------

Flamingo provides a small variety of value types, the most simple of which is the integer (for a complete list see the reference). You can do math on integers with the usual arithmetic operators `+`, `-`, `*`, `/`:

```
println + 1 2
println - 2 5
println * 9 3
println / 8 3
```

Flamingo uses Polish notation, meaning operators are written in front of their operands instead of in between. In fact, there is no difference between operators and functions in Flamingo. `+` is just a builtin function like `println`. Every function has a fixed number of arguments. When the parser tries to parse an expression and encounters a known function, it will parse as many expressions as are needed for the function as arguments. This is why there are no parentheses around the arguments of a function call.

You can also assign the results of expressions to variables with the `bind` function.

```
bind 'n 3
bind 'result + 1 * 2 n
```

`bind` takes an identifier as first argument. The quote character (`'`) is needed to prevent Flamingo from looking up the value of `result` which doesn’t exist yet because we are about to bind it! We are just interested in the name `result`. We can also simply bind a different value to `result` at a later time.

```
bind 'result "foo"
```

As you can see we bind the string `"foo"` to `result`. Variables in Flamingo have no type, there is no static typing. Only values have types and values of any type can be bound to variables.

Every variable has a so-called assoc-list. An assoc-list is just a list (another value type in Flamingo) where every element at an even index is an identifier and every element at an odd index is a value. An assoc-list is used to store further information about a variable, sort of like metadata. If we take a look at the assoc-list of `result` (using the builtin function `assoclist`), we can see that a variable starts out without any associated values. We can use the builtin function `assoc` to store any information we want.

```
assoc 'result 'age 78
println assoclist 'result
```

Note that the assoc-list is stored with the variable, *not* the value stored in the variable. Whenever you bind a value to a variable, `bind` clears the assoc-list to prevent stale assocs. If you want to keep the assoc-list, use the builtin function `store` instead.


Value Comments
--------

Value comments are surrounded by `{*` and `*}` and can be nested. Comments are first-class citizens and can be printed or bound to a variable just like any other value.

```
bind 'comment {* Looking good. *}
println comment
```

You can even create your own value comments at runtime via the builtin function `make-comment`. It expects 4 arguments since a value comment isn’t just a fancy string. It stores information about where in the source code it is located. The first three arguments to `make-comment` are a string representing the name of the source file, an integer representing the line, and an integer representing the column. The last argument is the contents of the comment. While you might assume that this argument must be a string, it, in fact, can be a value of any type. Value comments you type directly into a source file contain strings but you are not restricted like that when creating comments.

```
make-comment "make-comment" 1 1 "My first comment."
make-comment "included_file.fl" 10 13 (1 3 5)
```

Flamingo provides a few builtin functions to operate on value comments: `getloc` to retrieve the source location in form of an assoc list (`('source "included_file.fl" 'line 10 'col 13)`) and `peel` to retrieve the stored value.

Furthermore, there is the concept of a stashed comment. If a statement (basically, a line of code) contains only a value comment, Flamingo doesn’t just throw it away like it would do with integers or strings; it stores it. Functions can access this stashed comment to do whatever they want. `bind` uses this to store comments as documentation in variables if they are preceded by comments.

```
{* The anwer to life, the universe, and everything. *}
bind 'answer 42
println assoclist 'answer
```

If you split a value comment across several lines, they are automatically concatenated.

```
{* There is much to say about the variable foo. *}
{* But we don’t want to dwell on it. *}
bind 'foo 15
println assoclist 'foo
```

The stashed comment is cleared, when an empty line is encountered. Note that a comment is stashed in the current scope.

```
{* Just a wasted comment. *}

{* foo is alright. *}
bind 'foo "foo"
println assoclist 'foo
```

You can even store dynamic comments by using the builtin function `stash-comment`.


Test comments
-------------

If a statement is executed while a string comment is stashed that begins with "TEST ", Flamingo evaluates the expression after "TEST " and checks whether the expression the statement produces matches the test expression. If they are not equal, an error is raised and execution stops.

```
{* TEST 13 *} + 1 * 2 6
```

The stashed comment is cleared afterwards.

Another comment test directive is `TESTWITH`. It expects an identifier and two values (separated by whitespace). It binds the first value to the given identifier and afterwards compares the second expression with the statement result. What makes `TESTWITH` special is that a comment can contain several `TESTWITH` directives and the statement is executed for every directive. Imagine, for example, you really wanted to make sure that Flamingo doesn’t mess up multiplication with 0. You can test it like this:

```
{* TESTWITH x 0 0 *}
{* TESTWITH x 1 0 *}
{* TESTWITH x 2 0 *}
{* TESTWITH x 7 0 *}
{* TESTWITH x 12 0 *}
* x 0
```

Now, remember that comments can be dynamically created and stashed. This means you can dynamically create tests.

```
stash-comment make-comment "TEST" 1 1 "TEST 13"
+ 1 * 2 6
```

This is equivalent to our literal comment TEST above.

This is even more useful in conjunction with `TESTWITH`. There is a builtin function `testtable` that generates test cases for you. It takes a variable name, and two lists of values that are equal in size. For each element of both lists it generates and stashes a `TESTWITH` comment for the given variable and the corresponding input and expected value.

```
testtable 'x (0 1 2 3 4) (yes no yes no yes)
= mod x 2 0
```

For the specific case of number ranges the first list could be more easily generated with the builtin functions `iota` and `iota+`.


Switch comments
---------------

Another kind of comment in Flamingo is the switch comment. Switch comments are a single character surrounded by `{-` and `-}` (and optional whitespace). These switches are flags that govern the execution of Flamingo programs. The `T` switch for example enables the testing comments we’ve just seen (which is the default). If you use the `t` switch, testing is disabled.

```
{* TEST 5 *}
+ 2 3

{- t -}
{* TEST 6 *}
+ 2 3

{- T -}
{* TEST 5 *}
+ 2 3
```

There are two kinds of switch comments. The ones that govern the lexing and are enabled/disabled as soon as they are encountered, and the ones that produce tokens and are only acted on when executed. `u`/`U` is an example of a lexing switch. When enabled, it normalizes every identifier to be uppercase. Note that this will make identifiers (and therefore variables and builtins) that were created with lowercase letters unavailable. Further note that the builtins are lowercase. If you would like to work completely in uppercase, use the `-U` command line switch.

For every switch the uppercase version enables something while the lowercase version disables it. There is also a debugger available via the `b`/`B` switch. See the reference for information about the debugger and a list of all avaiable switches.


Control structures
------------------

Flamingo of course provides basic control structures. `if` comes in two versions: with `else` and without. The `if` condition must be of type bool (the bool literals are written `yes` and `no`):

```
if yes [
    println "Hello, world."
]

if no [
    println "Bye."
]

if yes [
    println "Print this."
    if no [
        println "But not this."
    ]
]

if no [
    println "This shouldn’t occur."
] else [
    println "But this should."
]
```

There is also a `for` loop that can iterate over lists.

```
for ch ('a 'b 'c) [
    println ch
]
```

It executes the block for every element of the given list which is bound to the given variable. If the variable is `_`, no value will be bound to it. The builtin function `iota` takes an integer _n_ and returns a list containing the natural numbers 0 ≤ _i_ < _n_. `iota` can be used with a `for` loop if you want to repeat an action several times. `iota+` is similiar but takes two arguments and returns the natural interval (excluding the right side).


Blocks
------

In the previous section we saw the use of blocks. Blocks are code that is enclosed in `[` and `]`. They are not executed when encountered but rather stored as a block value that can be executed at a later time with the builtin `eval` function. The result of this call is the last expression that was evaluted in the block. An empty block results in `no`.

```
bind 'block [ * 2 3 ]
println eval block
```

You can pass arguments to blocks via the `apply` builtin. They can be accessed from within the block via the `getparam` builtin.

```
bind 'twice [ * 2 getparam 0 ]
println apply twice (3)
```

`apply` takes a block and a list of values, and evaluates the block with the arguments.

There is another way to evaluate a block. To be called like regular builtin functions the parser needs to know how many arguments it should parse to pass to the block but a block doesn’t contain any information about how many arguments it wants. This information is easy to supply but it is not associated with the block value but with a variable instead. You can set the `'arity` associative value on a variable to tell the parser how many parameters a block takes.

```
bind 'inc [ + 1 getparam 0 ]
assoc 'inc 'arity 1
println inc 5
```

When a variable name is evaluted, its value is looked up in the current scope. If it is a builtin function or a block and the variable has an arity associated, the parser parses a function call. There is the `&` operator that is similiar in function to `'`. It lookups variables without trying to parse function calls, you always just get the stored value.

```
println &inc
```

Blocks can also be concatenated via the `+` builtin. A concatenated block just evaluates the statements of the first block and then the statements of the second block, just as if you had written them one after the other.

```
bind 'greeter [ println "Hello." ]
bind 'dec [ - getparam 0 1 ]
assoc 'dec 'arity 1
store 'dec + greeter &dec
println dec 5
```

That means you can modify the code stored in a variable after it has already been defined. Take the following example where we programmatically store the parameter in a variable:

```
bind 'square [ * x x ]
assoc 'square 'arity 1
store 'square + [ bind 'x getparam 0 ] &square
println square 3
```

If there was a macro facility, we would be able to write a macro to handle all the boilerplate.


Macros
------

Use the `macro` keyword to define a macro. It expects a name for the macro, the number of parameters and a body.

```
macro answer 0 [ 42 ]
```

If a macro is encountered, the parser consumes as many macro arguments as are specified. A macro argument is either a simple token or all the tokens between (balanced) parentheses and brackets. The arguments are inserted into the body of the macro and the result is evaluated. Note that the resulting body must be a valid list of Flamingo statements. You can’t use macros to generate only parts of a construct. Neither the body of the macro nor the arguments must be valid—they are just lists of tokens—as long as the final result is valid.

The simplest way to insert a macro argument is `,`_n_, a comma followed by an integer token. The integer must be greater or equal to zero and less than the number of parameters. The tokens of the given argument are spliced in-place.

```
macro printvar 1 [ println << ',0 ": " ,0 >> ]
```

This defines a simple macro that expects an identifier and results in a statement that prints the name and the value of the corresponding variable. (A similar macro named `debug` is builtin.)

Another use of arguments is `,len` _n_ (note that there must be a space between `len` and the integer). It inserts an integer token representing the number of tokens in the given argument.

```
macro count-tokens 1 [ println << ,len 0 " tokens" >> ]
```

Note that if you pass a token list (using parentheses or brackets) only the tokens between the outer parentheses or brackets are passed. There is also no way to find out if parentheses or brackets were used.

With this information we can already write a small macro that helps defining functions.

```
macro func 3 [
    bind ',0 [ ,2 ]
    assoc ',0 'arity ,len 1
]

func square (x) [ * x x ]
```

This macro binds the given block to a variable and associates the arity derived from the parameter list. Note the brackets around `,2`. Since only the contents of the block is passed to the macro we need to surround it with brackets.

This macro isn’t yet complete (and the execution of `square` would fail) since we don’t bind the numeric block arguments to the given parameter name `x`. For this we need loops.

`,for` expects an argument and a block. It inserts the evaluated body for every token in the given argument. In the body of the loop two new arguments are made available (that are just appended to the current list of arguments): the token of the current iteration and an integer token representing the loop index.

```
macro func 3 [
    bind ',0 [ ,2 ]
    assoc ',0 'arity ,len 1
    ,for 1 [
        store ',0 + [ bind ',3 getparam ,4 ] &,0
    ]
]
```

We iterate over the list of the parameters and add some code to the already stored function block. The code we preprend just binds the parameter via the index (macro argument `4`) to the parameter name (macro argument `3`). This is in fact the definition of the builtin macro `defun`.

---

Language reference
------------------

There are 21 types of tokens in Flamingo:

> ints, floats, identifiers, value comments, switch comments, strings, an empty line <br>
> `(` `)` `[` `]` `'` `<<` `>>` `&` `,` `if` `else` `for` `macro` `return`

Ints and floats can only be written in decimal notation (except when the `H` switch is enabled) with an optional leading `-`. Floats must start with a minus sign or a digit (`.5` is not a valid float literal).

Identifiers consist of A—Z, a—z, `_`, `+`, `-`, `*`, `/`, `<`, `>`, `=`, `.`, `?`, `!` and 0—9 but they cannot start with a digit. Note that `<<` and `>>` are not identifiers but their own token types.

Value comments are text enclosed in `{*` and `*}`. The text is arbitrary as long as `{*` and `*}` are balanced.

Switch comments are a single character enclosed in `{-` and `-}` (with optional whitespace). There are lexing switches that are enabled/disabled as soon as they are lexed, and there are regular switches that must be executed to be enabled/disabled. Lexing switches do not appear in the list of tokens. Non-lexing switch comments evaluate to values.

Strings are enclosed in `"`. Available escape sequences are: `\n`, `\t` and `\"`. Strings can contain newlines.

An expression consists of a value literal (ints, floats, strings, value comments, switch comments), an identifier, a list (several expressions surrounded by `(` and `)` separated by whitespace), a block (several statements surrounded by `[` and `]`) or a string-list (several expressions surrounded by `<<` and `>>`).

The expressions in a list are evaluated and a list value is created.

The statements in a block are not executed but stored in block value.

The expressions in a string-list are evaluated, converted to strings and concatenated resulting in a string value.

An identifier can be preceded by `'` to create an identifier value. When an identifier is encountered, its value is looked up. Flamingo is dynamically scoped. If the value either is a builtin or it is a block and the variable is associated with an `'arity`, a number of expressions following the identifier corresponding to the arity are parsed and evaluated. The builtin or the block is called with these arguments and the value is returned. If the value is a macro, a number of macro arguments corresponding to the macro’s number of parameters is read, the macro is evaluated and the resulting block is evaluated. An identifier can be preceded by `&` to force a simple lookup without trying to parse function or macro calls.

A statement is either an empty line, an `if`, a `for`, a `macro` definition, a `return` statement or an expression.

An empty line clears the stashed comment in the current scope. It evaluates to `no`.

An `if` is followed by an expression that must be of type bool and a code block. It can be followed by an `else` and another code block. If the condition is `yes`, the first block is evaluated producing the value of the `if` statement. If the condition is `no` and an `else` block is provided, this block is evaluated producing the value of the `if` statement. If the condition is `no` and no `else` block is provided, the statement evaluates to `no`.

A `for` is followed by an identifier, an expression and a code block. The expression must produce a list. The block is evaluated for every element of the list. It is evaluated in a new scope. The list element is bound to the given identifier if that identifier is not `_`. The value of the statement is the value of the block of the last iteration. If the list is empty, its value is `no`.

A `macro` statement is followed by an identifier, an integer token and a block. It creates a macro value that stores the integer which is the number of parameters the macro receives and the block, and binds that macro value under the given name in the current scope.

A `return` is followed by an expression and ends the execution of an function call early resulting in the expression value. An execution of a code block constitutes a function call if: its origin is `eval`, its origin is `apply`, its origin is a parsed function call due to a bound block value having an associated `'arity`.

If a statement is an expression that results in a value comment, this comment is stashed. See `stash-comment` for further semantics.

If a statement is an expression that results in a (non-lexing) switch comment, the corresponding switch is set in the current scope. A new scope inherits the switch values of its parent. Available switches are:

- `u`/`U`: Lexing switch that normalizes every identifier to be uppercase (default: off).
- `h`/`H`: Lexing switch that sets the number base to 16 (default: off).
- `t`/`T`: Enable or disable testing (default: on).
- `b`/`B`: Enable or disable step debugging (default: off).
- `f`/`F`: Enable or disable failing hard. If an error is encountered and failing hard is enabled, the execution immediately halts, even in the REPL. If failing hard is disabled, the error can be recovered with the `recover` builtin. If it is not recovered, execution will halt at the end of a statement (default: on in file execution and off in the REPL).
- `n`/`N`: Enable or disable narrating. When enabled, a statement is about to be executed and a comment is stashed, print the comment out to stderr (default: off).

If a statment is an expression and there is a stashed string comment beginning with "`TEST `" and testing is enabled, the expression following "`TEST `" is evaluated in the current scope and this test expression is tested for equality with the result of the statement of expression. If they are not equal, an error is raised. If there is a stashed string comment beginning with "`TESTWITH `" and testing is enabled, the statement is executed for every `TESTWITH` clause in the comment. A `TESTWITH` clause is followed by an identifier and two expressions. Both expressions are evaluated in the current scope. The result of the first expression is bound to the identifier in the current scope before the statement is executed. The result of statement is compared with the second expression for equality. If they are not equal, an error is raised.

When a variable that has a macro value bound to it is encountered, the parser consumes as many macro arguments as the macro has designated parameters. A macro argument is a regular token or several tokens enclosed between balanced parentheses and brackets. The outer parentheses/brackets are not part of the argument. To insert the arguments into the macro body every token that is not preceded by a `,` (comma) is copied to the output. A comma followed by an integer token copies the corresponding argument into the output. A comma followed by the identifier `len` and an integer token, writes an integer token to the output that is the length of the corresponding argument. A comma followed by the identifier `for`, an integer token and a body evaluates the body for every token in the argument. Two additional parameters are made available to the body: the token and the loop index as an integer token. When the output list of tokens is finished, it is executed as a block in the current environment.

When a statement is about to be executed and step debugging is disabled, a debugger command line is launched allowing you to introspect and manipulate the program state. The command and the arguments are separated by spaces. The following commands are available:

- `p`/`print`: Takes an expression as argument. Print the result of the expression in the current scope.
- `set`: Takes a name and an expression as arguments. Bind the result of evaluating the expression in the current scope to the name in the current scope.
- `peek`: Takes a number as argument. Print that many tokens that are next to be executed.
- `skip`: Takes a number as argument. Skip that many tokens that are next to be executed.
- `n`/`next`: Execute the next statement but launch the debugger before the execution of the statement after that.
- `cont`/`continue`: Disable step debugging and continue the execution as normal.
- `exit`: Halt the program execution.

Builtins
--------

If a builtin is called with arguments of the wrong type, an error is raised.

`yes`, `no`
> Bool literals.

`println val:value`
> Prints `val` to stdout followed by a newline.

`bind name:ident val:value`
> Bind `val` to `name` in the current scope and clear the assoc list of `name`. Also see `store`.

`store name:ident val:value`
> Bind `val` to `name` in the inner most scope that contains `name`. If no scope contains `name`, create it in the current scope. Doesn’t touch the assoc-list of `name`.

`assoc name:ident slot:ident val:value`
> Set `slot` to `val` in the assoc-list of `name`.

`assoclist name:ident`
> Return the assoc-list of `name`.

`+ a:int b:int` <br>
`+ a:int,float b:int,float` <br>
`+ a:block b:block` <br>
`+ a:list b:list`
> If `a` and `b` are integers, add them as integers and return an integer. If one or both are float, convert both to float and return the sum.
> Concatenate blocks and lists.

`- a:int b:int` <br>
`- a:int,float b:int,float`
> If both arguments are integers, subtract them as integers. Otherwise subtract them as floats.

`* a:int b:int` <br>
`* a:int,float b:int,float`
> If both arguments are integers, multiply them as integers. Otherwise multiply them as floats.

`/ a:int b:int` <br>
`/ a:int,float b:int,float`
> If both arguments are integers, subtract them as integers. Otherwise divide them as floats.

`/. a:int,float b:int,float`
> Divide `a` and `b` as floats.

`mod a:int b:int` <br>
`mod a:int,float b:int,float`
> If both arguments are integers, return the modulus of an integer division. Otherwise return the modulus of a float division.

`= a:value b:value`
> If `a` and `b` are of different types, they are not equal. Integers, floats, strings, identifiers and lists have obvious equality. Value comments compare their source location as well as their contained value. Switch comments compare the switch and the state. Blocks and macros are never equal to each other (not even to themselves).

`<> a:value b:value`
> Negation of `= a b`.

`<  a:int b:int` <br>
`<  a:int,float b:int,float` <br>
`<= a:int b:int` <br>
`<= a:int,float b:int,float` <br>
`>= a:int b:int` <br>
`>= a:int,float b:int,float` <br>
`>  a:int b:int` <br>
`>  a:int,float b:int,float` <br>
> If both arguments are integers, compare them as integers. If one of them is float, compare them as floats.

`and a:bool b:bool`
> Return `yes` if both arguments are `yes`, otherwise `no`.

`or a:bool b:bool`
> Return `yes` if one or both arguments are `yes`, otherwise `no`.

`not a:bool` <br>
`not c:switch-comment`
> If the argument is bool, negate it. If the argument is switch comment, return a switch comment with negated state.

`->string a:value`
> Convert `a` to a string.

`getloc c:value-comment`
> Return an assoc-list containing the source location of `c`. The keys are: `'source` (a string), `'line` (an integer) and `'col` (an integer).

`peel c:value-comment`
> Return the contained value of `c`.

`make-comment src:string line:int col:int val:value`
> Create a value comment.

`stash-comment c:value-comment`
> Stash `c` in the current scope. If `c` already contains a comment, the values of both comments are converted to string, concatenated with a newline character in between, and a new comment with the source location of the first comment and the combined value is stashed.

`switch? c:switch-comment`
> Return a bool whether `c` contains an enabled or disalbed (non-lexing) switch. This does not query the environment but just `c`.

`switch! c:switch-comment`
> Enable/disable the (non-lexing) switch in the current scope according to `c`. Return a bool whether the switch is enabled in the current scope.

`env-switch? c:switch-comment`
> Return a bool whether the (non-lexing) switch corresponding to `c` in the current scope is enabled. The state of `c` is ignored.

`testtable name:ident inputs:list expected:list`
> `inputs` and `expected` must match in size. Create and stash a `TESTWITH` comment for the given name and every pair of elements of `inputs` and `expected`.

`iota n:int`
> Return a list containing the integers 0 ≤ _i_ < `n`.

`iota+ a:int b:int`
> Return a list containing the integers `a` ≤ _i_ < `b`.

`eval b:block`
> Execute `b` in the current scope and return the value of the last statement of `b`. If `b` is empty, return `no`. Also see `apply`.

`apply b:block args:list`
> Execute the `b` in a new scope that has `args` set as parameters and return the value of the last statement of `b`. If `b` is empty, return `no`. Also see `eval`.

`recover b:block`
> Execute `b` in the current scope. If an error occurs during the execution, return a value comment containing the location of the error and the error message. Otherwise, return the value of the block.

`getparam i:int`
> Retrieve the `i`th parameter (zero-based) of the current scope. Only meaningful in a block execution and if the block has been provided with parameters. See `apply`.

`floor x:float`
> Return the greatest integral value (as float) that is smaller than x.

`sqrt x:float`
> Return the square root of `x`.

`sin x:float`
> Return the sine of x.

`cos x:float`
> Return the cosine of x.

`float->int x:float`
> Truncate the value of `x` (round towards to zero) and return it as an integer.

`type v:value`
> Return an identifier designating the value type of `v`, one of: `'bool`, `'int`, `'float`, `'ident`, `'bool`, `'string`, `'list`, `'value-comment`, `'switch-comment`, `'builtin`, `'block`, `'macro`.

`len xs:list`
> Return the integer length of `xs`.

`at xs:list idx:int`
> Return the element at index `idx` (zero-based). If the index is out of range, raise an error.

`error msg:string`
> Raise an error at the location of the call with the given message.

`defun name:ident params:list body:block`
> `defun` is a macro. `params` must be a list of (non-quoted) identifiers. `defun` binds the code block to the given name, associates the `'arity` derived from the parameter list, and inserts code in the code block to assign the numeric arguments to the given parameter names.

`debug vars:list`
> `debug` is a macro. `vars` must be a list of (non-quoted) identifiers. `debug` prints out each of the named variables alongside its value.