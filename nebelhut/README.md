🦩 Flamingo
========

Getting started
--------

To build the interpreter you can call `build.sh` or just compile `flamingo.cpp` with your favorite C++ compiler. (You need to that in this directory as `flamingo.cpp` includes some files.) You can call `flamingo` with a filename to execute the entire file; or without arguments to launch the REPL.

To print something to `stdout` you can use the builtin function `println`:

```
println "Hello, world."
```


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

Comments are surrounded by braces (`{` and `}`). Comments are first-class citizens and can be printed or bound to a variable just like any other value.

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