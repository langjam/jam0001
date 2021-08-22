# Suslang

Suslang is a small language with Rust-like syntax that is focused on Meta-Programming.
It has some support for first-class comments in the ["first-class citizen"](https://en.m.wikipedia.org/wiki/First-class_citizen) sense, for example you can write
```rs
let x = /* Hello, world! */;
print(x);
let y = // Hello, langjam!;
print(y);
```
and if you run this program it will print
```
/*  Hello, world!  */
/*  Hello, langjam! */
```

Apart from that, it supports 
 * basic mathematical expressions with precedence, 
 * functions and classes, 
 * `for elem in list` loops and `if` / `else if` conditionals (but no `else` without `if`)
 * lists, written `[x, y]`,
 * objects `Class{ field: value }`, and
 * exactly those built-in functions that I needed to make this work. These are listed in the [Built-In Functions and Methods section](#built-in-functions-and-methods).
 

You can check the type of any element by asking for its `.class`.


**If you only look at one thing from this language, then make that thing [the Build Script section](#build-scripts) and [the `build_check_comment` example](#getting-creative-with-comments).**


## Build Scripts

The real power of Suslang comes from its build scripts.
When you run a file, the program you pass is **not** the source file you want to run in the end.
Instead, you pass a Suslang program that defines a `build` method, which is responsible for any further action that is part of running your program (or, alternatively, not running your program. See `examples/dont_build`).

We expose methods to `read` Suslang files, `parse` them to get their [abstract syntax tree (AST)](https://en.m.wikipedia.org/wiki/Abstract_syntax_tree), and `eval` ASTs to run a program.
You can inspect and modify ASTs returned by `parse` like any other object in the language, giving you the ability to **meta-program** the program you will be running.
You can change values, remove or add statements, or do whatever else you can think of (we do have some [Examples](#examples))!

**Any comments, be they values like above or just comments that appear in the parsed file, are preserved in the AST.**
This means that even comments that are not first-class in the program that you run **can be handled as first-class values in your build script** through their AST nodes.
The same applies of course to any other AST nodes, but we show some ways to make use of comments below.

Build script `build` methods must have a single parameter, which will be instantiated with a list of the arguments passed to an invocation of our runner (see [usage](#try-it-out)).

## Examples

The `examples/` directory contains a few Suslang source files.
Only two of these files, `print.sus` and `comment.sus` are intended to be used as actual programs.
All of the other files, that all have `build` in their name, are build scripts that handle these two programs in different ways.

To start, we have the `just_build.sus` build script which, apart from checking for errors, will take in program paths and run these programs without any mangling or modification.
All of our example build scripts in `examples/` take their arguments to be a list of files to run (see [usage](#try-it-out) for details on how to run a program).
Of course you can (and please do!) always be more creative with additional flags or values.

If you run `just_build` on `print` and `comments`, you will see some status output from the build script and then the result of each program evaluation.
`comments.sus` will print some nice, first-class comments, but otherwise nothing to exciting.
However, build scripts are a very tempting place to **comment** on what you think of the program you are supposed to run and, if you are not pleased with it, maybe re-consider running it.
The `dont_build.sus` build script will do just that - no matter what file(s) you pass it, it does not like them and instead does some counting, which it much prefers.

## Real Meta-Programs
To show off a bit more of just how much Suslang can do, let's go through the `examples/` build scripts that start with `build_`.
The name alone shows how dead serious these scripts are about their job!

The `build_parse_string` script does not take input, but instead parses two Suslang programs from memory.
It just passes their source directly to the `parse` function.
One of its programs is erroneous and will produce an error, the other will run successfully.

The `build_modify_let` script _will_ run the programs you pass it, but wherever you have `let var = value;` statements, it will replace `value` with a new expression that adds `1` to it before evaluation.
When the program you passed is running, every `let` statement written will instead by `let var = value + 1;`, including where `value` is itself a more complicated expression.
Be careful, if you have `value`s that aren't really all that arithmetical, this script might cause some headaches!

With `build_your_own_program`, we are back to build scripts that don't care about their input. 
This one is a lot like `build_parse_string`: it also makes up its own program as it runs.
But unlike `build_parse_string`, here we **do not use `parse`**. 
The new program is made entirely from AST definitions (see the [list of such definitions](#ast-classes)), which are then passed to `eval`.

### Getting Creative With Comments

We have saved the best for last.
If this is the only thing you try, I'll be happy to have shown you the most real-world useful and complicated build script I managed to write and make work during the jam.
`build_check_comments.sus`.

This is a build script that will run _your_ programs.
It runs them the way you wrote them, and it doesn't do anything silly like being rude or sneaking in random modifications.
But before it runs them, **it will check your programs for comments you have made and will react to them** in multiple ways.

If you try it, the example is intended to be used on the `comments.sus` input file.
That file contains several comments, both as regular comments and as first-class values, some of which contain additional `@tag` tags.
When you run the example, you will get a warning for any `@todo` or `@fixme` tags in `comments.sus`, one of which is in a comment assigned to `y`.

`comments.sus` also contains a single function, `has_comments(x)`.
We have tagged this function with a `// @trace` comment in the line before it.
**Because of this comment, you will see whenever `has_comments` is called, and which value of `x` it is called with.**
None of this is a part of our parser or evaluator, nor is it written in `comments.sus` in any way other than the comment tag.
This is all happening in the `build_check_comments` build script.

The way it works is that the build script looks through the entire source file and remembers where it has seen functions annotated with a `@trace` tag.
When processing the definitions of these functions, **it will then insert code that `print`s the tracing information right into the function body**, as the first thing to happen when the function is called.
When the modified program is `eval`uated, this code runs and puts the tracing info on the console.

Not that we use the `ref` keyword when passing AST objects as arguments to prevent them from being evaluated.

## Try It Out
Our implementation is written in Rust, so you will need to have the Rust compiler installed.
If you install Rust, or already have Rust on your system, you will also have the `cargo` build tool and package manager.

From this folder, simply run 
```shell
> cargo run -- --build <path to build script> [build arguments]...
```

Any `build arguments` will be passed to the build script as the `args` parameter of the `build` function.
Since all of our build scripts take a list of files to run as their arguments, to run of our `examples/` scripts, just pass it a list of original programs to execute.
For example, to run both the `print` and the `comments` file with the default build script, run
```shell
> cargo run -- --build examples/just_build.sus examples/print.sus examples/comments.sus
```

The program supports showing usage help.
To see that, type
```shell
> cargo run -- --help
```

## Built-In Functions and Methods

The following is a list of all available built-in functions:
 - `print(a, b, c, ...)`, which takes any number of parameters and outputs them sequentially on a single line
 - `range([from,] to)`, which can take one or two integer arguments and produces a list containing the values from the first (or `0`) to the second
 - On `String`s:
     * `String.len`, to get the length of the `String`
     * `String.contains(pattern)`, where `pattern` is also a `String`, returns a `Bool` that is `true` if `pattern` appears anywhere in the `String`
     * `String.starts_with(pattern)`: like `contains`, but `pattern` has to match at the beginning of the `String`
     * `String.split([pattern])`, which splits the `String` into a `List` of `String`s at the given `pattern`. If no `pattern` is given, it splits at sequences of whitespace.
 - On `List`s:
     * `List.len`, to get the length of the `List`
     * `List.contains(value)` returns a `Bool` that is `true` if the `List` contains an element equal to `value`
     * `List.add(value)` inserts `value` at the end of the `List`
 - `read(path)`, where `path` is a `String`, returns the contents of the file at `path` as a `String`. If the file does not exist or cannot be read, it returns an `Error` object instead, which contains more info in its `msg` field.
 - `parse(input, name)`, where both parameters are `String`s, parses the given `input` and returns an AST object hierarchy with the given `name`. You can find a list of possible AST objects in the [AST Classes section](#ast-classes). If there are parse errors, it returns an `Error` object instead, which contains more info in its `msg` field.
 - `eval(ast)`, where `ast` is an AST object, runs our evaluator on the given input. If you don't perform any modification in your build script and there are no errors, `eval(parse(read(path), name))` should run the program at `path` as written.

 ## AST Classes
 - `ASTBoolLiteral`, `ASTIntLiteral` and `ASTFloatLiteral` each have a `value` field of the corresponding type
 - `ASTStringLiteral` and `ASTComment` have a `text` field that contains their content
 - `ASTIdent` has the `name` that is referenced, a `String`
 - `ASTListLiteral` has a list of its `elements`
 - `ASTBinaryExpr`
    * `op`: a `String` representation of the operator, e.g. `"+"` for addition
    * "lhs" and "rhs": AST objects of the left-hand side and right-hand side
 - `ASTRefExpr` represents `ref value` expressions and has a corresponding `value` field, which is an AST object
 - `ASTObjectLiteral`s have a `class` (which is a `String`) and a `List` of `fields`, which are `ASTFieldLiteral`s
 - `ASTFieldLiteral`s have a `String` `name` and a `value` which is an AST object
 - `ASTForLoop`s `for var in list {..}` give acces to the`var_name` of the iteration variable (`var`, a `String`), their `target` expression AST object (a node representing `list`), and their `body`, which is a `List` of further AST objects representing statements
 - `ASTIfStmt`s have a `condition` AST node, a `body` `List` of AST statements, and a `List` `elses` of `else if` clauses
 - `ASTFnCall`s have a `name` `String` and a `List` of `args`
 - `ASTLetStmt`s assign a `value` AST object to a variable with `String` name `var_name`
 - `ASTRetStmt`s return a `value` AST object
 - `ASTFnDef`s have the function `name` (`String`), a `List` `params` of the `String` names of their parameters, and a `List` of AST `body` statements
 - `ASTClassDef`s have a `String` `name`, a `List` of `String` field names `fields`, and a `List` of `methods`, which contains `ASTFnDef`s
 - `ASTFile` has a `String` `name` and a `List` of AST items `elements`

 Remember that you can always ask objects for their `.class`!