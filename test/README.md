# 'AAA'

## The name, and the idea

The name is still a placeholder, but it seems like a reasonable enough name, so it shall be named just that.

Comments are, in the usual sense, descriptions of a particular part of code, so why not harness this power to refer to
said parts of the code?
In this language, comments are first-class...containers for identifiers and descriptions, never again be limited to mere
economy-class identifiers that point to one value and can only be one word!

That said, the language does still allow identifiers and variables, in case that's simpler to use.

## Build steps
The typical C++/CMake routine, the only dependency is a compiler that supports C++2a.
```shell
$ mkdir build
$ cd build && cmake .. && make
```

The built binary is named `test`.

## Examples
There are a few examples provided, an implementation of fibonacci, and test file that showcases most of the features, and
a few other tidbits.
To run a file, simply pass it as the only argument to `build/test`, for instance,
```shell
$ build/test examples/fib.aaa
```

## The language
### General syntax
A program may contain any number of expressions or assignments, each described by zero or more comments.

Expressions can be variables, literals, anonymous functions, function calls, record type expressions, member access, or a
_comment mention_; the syntax follows:
- variables
    Any _identifier_, which can contain any non-meaningful character (any character except `:(){}<>!"=;,.|`, and whitespace)
    optionally followed by a colon and an expression denoting its type (e.g. `foo: int` or `foo: bar.baz(int)`)
- literals
    Either numbers, or very simple double-quoted strings.
- anonymous functions
    A code block of the form `{ parameters? return? body }`, where parameters are any number of variables surrounded by
    pipe characters (`|`), and the return value is a colon (`:`) followed by a variable.
    The return value of a function (when called) is determined by the last value assigned to the return variable.
    for instance, `{ |a: T|: x let x = a; }` will take a single parameter of type 'T', and return the same value when called.
    Note that such functions are proper closures, so they may refer to variables in the environment they were created in.
- function calls
    Any expression can be _called_, by providing it a series of _arguments_ in parentheses: `expr(arg0 arg1)`.
    Note that commas in the argument list are optional, and may be used to disambiguate expressions such as `fn(arg0 (arg1))`
    where `fn(arg0, (arg1))` is meant.
    Note that calling a _type_ will produce a value of that type.
- Comment mentions
    Values that are described by comments can be referred to by the contents of their comments using the `<...>` syntax
    (or `!<expression>` syntax, when dynamic lookup is needed).
    Such expressions produce a set of results, and operations on them will operate on _all_ results in the set.
    For instance, calling a comment mention that matches four values will produce four values.
- Record types
    User-defined structures can be defined using the `record { <variable>* }` syntax, which produces a _type_.
    The variables in the record may have types, or may omit the type to default to _any_.
    Note that these are still values, and may be passed to functions, or returned from them.
- Member access
    An expression of the form `<expr>.<identifier>` is an access to the member named by <identifier> in the <expression>.
    Currently, record values respond to this by the respective field, string values have a `length` member, and numeric
    values have a `negated` member.

Assignments are constructed as `let <variable> = <expression>`, and always produce a binding on the topmost scope.

## Control Flow
Control flow is provided by two standard library functions `cond` and `loop`.

`cond` takes a list of alternating conditions and values, followed by a default "else" value, and resolves to the first
value whose condition evaluates as truthy (falsey values are empty strings, the number 0, and empty comment resolution sets).

`loop` takes an initial value, a step function, and a stop function.
It will keep applying the `step` function until the `stop` function returns a truthy value.

Note that as argument evaluation is eager, to postpone the evaluation of a value, one can pass functions to `cond` and `loop`,
and call the results at a later time.


## Surprising semantics
- Empty values can be created by the empty comment mention `<>`, or assignments with non-matching types, such values will
propagate without error.
- Calculations on comment resolution sets will produce more comment resolution sets, to "collapse" the set (so to speak), one
may use the `collapse` function to randomly select an element of the set.

## Standard functions
| | signature | operation | declared comments |
| :- | :-- | :-- | :--- |
| `print` | `print(...)` | prints out the given arguments, interspersed by spaces to stdout | `native print function operation` |
| `add` | `add(...)` | adds all the given arguments, respecting types | `native arithmetic addition operation` |
| `sub` | `sub(...)` | subtracts all the given numeric arguments | `native arithmetic subtraction operation` |
| `mul` | `mul(...)` | multiplies all the given numeric arguments | `native arithmetic multiplication operation` |
| `div` | `div(...)` | folds all the given numeric arguments by division | `native arithmetic division operation` |
| `mod` | `mod(...)` | folds all the given numeric arguments by modulo | `native arithmetic modulo operation` |
| `gt` | `gt(...)` | folds all the given numeric arguments by greater-than | `native comparison greater_than operation` |
| `eq` | `eq(...)` | folds all the given numeric arguments by equals | `native comparison equals operation` |
| `cond` | `cond(a b... e)` | selects from a series of alternatives by their condition | `native conditional selection operation` |
| `loop` | `loop(init step stop)` | applies `step` until `stop(accumulator)` is true | `native loop flow operation` |
| `is` | `is(value string)` | checks whether `value` would be selected by a comment mention of the value of `string` | `native comment query operation` |
| `collapse` | `collapse(value)` | selects a random member of the CRS in `value` | `native collapse flatten operation` |

## Standard types
| name | meaning |
| `int` | 32-bit signed integer |
| `string` | arbitrary-length string |
| `any` | any type, standard or user-defined |
