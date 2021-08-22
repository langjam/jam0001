# boros

<img src="Assets/Ouroboros.png" width="150" height="150" />

`boros` (Shortened from [Ouroboros](https://en.wikipedia.org/wiki/Ouroboros)) is a functional programming language that supports `first-class comments`, developed during the [2021 Lang Jam](https://github.com/langjam/jam0001).

## Build instructions

### Prerequisites

* GHC (>= 8.10.5)
* cabal (>= 3.4.0.0)

(Can be installed via [ghcup](https://www.haskell.org/ghcup/))

### Build the interpreter

```sh
cabal new-build
```

### Run the interpreter

```sh
cabal new-run . -- <script file> <args for boros script>...
```

e.g.

```sh
cabal new-run . -- Examples/Factorial.brs 100
```

## VS Code Extension

This repo also includes a VS Code extension for syntax highlighting.

You can install it by running the `install-vscode-ext.sh` script.

## Language features

`boros` is a strict, impure, dynamically-typed functional language with ML-inspired syntax.

[*(Jump to examples)*](Examples)

### Syntax

The entire `boros` script file is one big expression, so there's no need for a `main ()` function.

Variables and functions are declared with `let`, and multiple mutually-recursive functions can be declared with `let ... and`.

Here's a simple factorial program:

```haskell
let fact x =
  if not x then
    1
  else
    x * fact (x - 1)
in

let n = 10 in
fact n
```

Values in `boros` are immutable, with the exception of *lists* and *records*.

Here's how to define and use them:

```haskell
let l = [1, 2, 3] in

l.[0] <- "abc";
print l.[0];

let r = { x = 2, y = 3 } in

r.x <- r.x + 1;
r.z <- 4; {- "You can even add new fields to an existing record!"; -}
```

### First-Class Comments

`boros`  comments are delimited by `{-` and `-}`. Currently, there's no support for line comments or nested comments.

In `boros`, comments *are interpreted as part of the source code*.

Furthermore, `boros` scripts have access to a value called `comments`, which contains the list of all comments in the source file.

The script can modify this list, and when the script finishes, the comments *are replaced with the new values from the `comments` list*, and *the script is re-run*. This continues until either the script stops modifying the comments, or calls the `halt ()` function.

A good example of this can be found in the [Fibonacci](Examples/Fibonacci.brs) example.

### Types and Exceptions

`boros` is dynamically-typed. Performing operations on values of the wrong type will halt the program with a `Runtime error`.

You can use the `type` function to get the "kinda sorta" type of a value, e.g. `type 2 == "Num"`, `type [1, "abc"] == "List"`.

You can also throw exceptions using the `throw` function. Any value can be provided as parameter, and will be `show`n in the error message. There is currently no `catch`/`finally` mechanism.

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).
