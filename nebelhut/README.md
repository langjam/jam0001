🦩 Flamingo
========

Getting started
--------

To build the interpreter you can call `build.sh` or just compile `flamingo.cpp` with your favorite C++ compiler. (You need to that in this directory as `flamingo.cpp` includes some files.) You can call `flamingo` with a filename to execute the entire file; or without arguments to launch the REPL.

To print something to `stdout` you can use the builtin function `println`:

```
println "Hello, world."
```

An overview over the syntax and all functions is given at the end.


Values
------

Flamingo provides a small variety of value types, the most simple of which is the integer. You can do math on integers with the usual arithmetic operators `+`, `-`, `*`, `/`:

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

`bind` takes an identifier as first argument. The quote character (`'`) is needed to prevent Flamingo from looking up the value of `result` which doesn’t exist yet because we are about to bind it! We are just interested in the name `result`. We can also simply rebind the value at a later time.

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


Comments
--------

Comments are surrounded by braces (`{` and `}`) and can be nested. Comments are first-class citizens and can be printed or bound to a variable just like any other value.

```
bind 'comment { Looking good. }
println comment
```

You can even create your own comments at runtime via the builtin function `make-comment`. It expects 4 arguments since a comment isn’t just a fancy string. It stores information about where in the source code it is located. The first three arguments to `make-comment` are a string representing the name of the source file, an integer representing the line, and an integer representing the column. The last argument is the contents of the comment. While you might assume that this argument must be a string it, in fact, can be a value of any type. Comments you type directly into a source file contain strings but you are not restricted like that when creating comments.

```
make-comment "make-comment" 1 1 "My first comment."
make-comment "included_file.fl" 10 13 (1 3 5)
```

Flamingo provides a few builtin functions to operate on comment value: `getloc` to retrieve the source location in form of an assoc list (`('source "included_file.fl" 'line 10 'col 13)`) and `peel` to retrieve the stored value.

Furthermore, there is the concept of a stashed comment. If a statement (basically, a line of code) contains only a comment, Flamingo doesn’t just throw it away like it would do with integers or strings; it stores it. A stored comment doesn’t do anything but functions can access this stashed comment to do whatever they want. `bind` uses this to store comments as documentation in variables if they are preceded by comments.

```
{ The anwer to life, the universe, and everything. }
bind 'answer 42
println assoclist 'answer
```

If you split a comment across several lines, they are automatically conacatenated.

```
{ There is much to say about the variable foo. }
{ But we don’t want to dwell on it. }
bind 'foo 15
println assoclist 'foo
```

The stashed comment is cleared, when an empty line is encountered.

```
{ Just a wasted comment. }

{ foo is alright. }
bind 'foo "foo"
println assoclist 'foo
```

You can even store dynamic comments by using the builtin function `stash-comment`.


Test comments
-------------

If a statement is executed while a string comment is stashed that begins with "TEST ", Flamingo evaluates the expression after "TEST " and checks whether the expression the statement produces matches the test expression. If they are not equal, an error is raised and execution stops.

```
{ TEST 13 } + 1 * 2 6
```

The stashed comment is cleared afterwards.

Another comment test directive is `TESTWITH`. It expects an identifier and two values (separated by whitespace). It binds the first value to the given identifier and afterwards compares the second expression with the statement result. What makes `TESTWITH` special is that a comment can contain several `TESTWITH` directives and the statement is executed for every directive. Imagine, for example, you really wanted to make sure that Flamingo doesn’t mess up multiplication with 0. You test it like this:

```
{ TESTWITH x 0 0 }
{ TESTWITH x 1 0 }
{ TESTWITH x 2 0 }
{ TESTWITH x 7 0 }
{ TESTWITH x 12 0 }
* x 0
```

Now, remember that comments can be dynamically created and stashed. This means you can dynamically create tests.

```
stash-comment make-comment "TEST" 1 1 "TEST 13"
+ 1 * 2 6
```

This is equivalent to our literal comment TEST above.

This is even more useful in conjunction with `TESTWITH`. There is a builtin function `testtable` that generates test cases for you. It takes a variable name, two integers representing a lower and upper bound and a list of values. For every integer in the given range it generates and stashes a `TESTWITH` comment for the given variable and the corresponding element of the list.

```
testtable 'x 0 4 (yes no yes no yes)
= mod x 2 0
```


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

It executes the block for every element of the given list which is bound to the given variable. If the variable is `_`, no value will be bound to it. There is a builtin function `iota` that takes an integer _n_ and returns a list containing the natural numbers 0 ≤ _i_ < _n_. `iota` can be used with a `for` loop if you want to repeat an action several times.


Blocks
------

In the previous section we saw the use of blocks. Blocks are code that is enclosed in `[` and `]`. They are not executed when encountered but rather stored as a block value that can be executed at a later time with the builtin `eval` function. The result of this call is the last expression that was evaluted in the block. An empty block results in `no`.

```
bind 'block [ * 2 3]
println eval block
```

You can pass arguments to blocks via the `apply` builtin. They can be accessed from within the block via the `geparam` builtin.

```
bind 'twice [ * 2 getparam 0 ]
println apply twice (3)
```

`apply` takes a block and a list of values, and evaluates the block with the arguments. `eval` is therefore equivalent to `apply` with an empty argument list.

There is another way to evaluate a block. To be called like regular builtin functions the parser needs to know how many arguments it should parse to pass to the block but a block doesn’t any information about how many arguments it wants. This information is easy to supply but it is not associated with the block value but with a variable instead. You can set the `'arity` associative value on a variable to tell the parser how many parameter a block takes.

```
bind 'inc [ + 1 getparam 0 ]
assoc 'inc 'arity 1
println inc 5
```

When a variable name is evaluted, its value is lookup in the current environment. If it is a builtin function or a block and the variable has an arity associated, the parser parses a function call. There is the `&` operator that is similiar in function to `'`. It lookups variables without trying to parse function calls, you always get just the stored value.

```
println &inc
```

Blocks can also be concatenated via the `+` builtin. A concatenated block just evaluates the statements of the first block and the statements of the second block, just as if you had written them one after the other.

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

Use the `macro` keyword to define a macro. It expects a name for the macro, the nubmer of parameters and a body.

```
macro answer 0 [ 42 ]
```

If a macro is encountered, the parser consumes as many macro arguments as are specified. A macro argument is either a simple token or all the tokens between (balanced) parentheses and brackets. The arguments are inserted into the body of the macro and the result is evaluated. Note that the resulting body must be a valid list of Flamingo statements. You can’t use macros to generate only parts of a construct. Neither the body of the macro nor the arguments must be valid—they are just lists of tokens—as long as the final result is valid.

The simplest way to insert a macro argument is `,`_n_, a comma followed by a integer token. The integer must be greater or equal to zero and less than the number of parameters. The tokens of the given argument are spliced in-place.

```
macro debug 1 [ println << ',0 ": " ,0 >> ]
```

This defines a simple macro that expects an identifier and results in a statement that prints the name and the value of the corresponding variable.

Another use of arguments is `,len` _n_ (note that there must be a space between `len` and the integer). It inserts an integer token representing the number of tokens in the given argument.

```
macro count-tokens 1 [ println << ,len0 " tokens" >> ]
```

Note that if you pass a token list (using parentheses or brackets) only the tokens between the parentheses or brackets are passed. There is also no way to find out if parentheses or brackets were used.

With this information we can already write a small macro that helps defining functions.

```
macro func 3 [
    bind ',0 [ ,2 ]
    assoc ',0 'arity ,len 1
]

func square (x) [ * x x ]
```

This macro binds the given block to a variable and associates the arity derived from the parameter list. Note the brackets around `,2`. Since only the contents of the block is passed to the macro we need to surround it with brackets.

This macro isn’t yet complete (and the execution would fail) since we don’t the numeric block arguments to the given parameter name `x`. For this we need loops.

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

We iterate over the list of the parameters and add some code to the already stored function block. The code we preprend just binds the parameter index (macro argument `4`) to the parameter name (macro argument `3`). This is in fact the definition of the built-in macro `defun`.


Reference
---------

String list: `<<` val... `>>`
You can enclose several values (separated by whitespace) in `<<` and `>>`. They are evaluated, converted to string and concatenated.