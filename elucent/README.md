# Smålang

A minimal language in runtime and semantics.

## Build

Requires GNU assembler.

```make
make smal	# to build
./smal		# to run
```

## Overview

Smålang is a very simple language. I was inspired by simple esoteric languages
like BF where there's no parser or lexer, just a tape of characters. Normally these
languages are very basic and not very useful (so-called turing tarpits), but I
like the concept of a language where there is really nothing other than evaluation.
Another small but highly enjoyable aspects of these languages is that any code that
doesn't get evaluated is typically ignored, meaning comments can be written in plain
english, so long as they aren't visited by the interpreter.

### Basic Data Structures

The smålang runtime consists of a few main parts:

 * A value stack, where values can be pushed and popped.

 * A heap, for allocating dynamically-sized values like strings and closures.

 * A variable table, mapping variable names to values across the whole program. Smålang is dynamically scoped.

 * A source file, where the smålang program as-written is stored.

 * An instruction pointer, indicating the next instruction to be evaluated, which can point into either the source file or the heap.

 * A call stack, where return addresses for the instruction pointer can be stored.

### Instructions

The "instruction set" of smålang is essentially just the ASCII character set. Each
character maps to an operation, and there are only a few possible operations characters
can have.

* Most letters, symbols, and digits will push themselves onto the stack as a character.

    * So, for example, `x` pushes the character 'x' onto the stack.

* Whitespace characters (spaces, tabs, line breaks) have a "push down" operation. Starting
from the top of the stack, character values are popped off, concatenated, and evaluated.
  * If the first character is a digit, the digits   are concatenated in base 10. So `123 `
  evaluates to the number `123`.

  * Otherwise, all characters on the top of the   stack are concatenated into a string. So `abc `
  evaluates to the string `"abc"`. If the string is the name of a previously-defined variable, that variable's value is pushed instead.

 * Control characters (most notably the null character) will first perform a "push down" operation, then will return from the current function. If there is no current function, the program halts.

 * `[` and `]` are "quote" and "unquote" respectively. Text between these characters is not evaluated, and "unquote" pushes this text onto the stack as a string.

 * `(` and `)` are "open" and "close" respectively. `(` opens a sub-stack, preventing operations within the parentheses from interacting with existing values on the stack.

 * `.` is the "fence operation". It places a fence on the stack that cannot be removed, effectively barring off all the values pushed prior to the fence.

### Values

As you may have guessed, smålang is a concatenative language. Values are pushed onto the stack, and functions can be pushed onto the stack to apply to the values there.

There are seven types of values in smålang:

 * Characters, representing individual ASCII symbols.

 * Integers, representing 32-bit signed integers.

 * Strings.

 * Native functions, which are defined by the interpreter.

 * User functions, which are defined by the user.

 * Booleans (true or false).

 * Fences.

### Application

The last major aspect of smålang's evaluation model is function application. Unlike most other concatenative languages, smålang uses neither prefix nor postfix notation: it uses _both_: whether we push a function onto a value, or a value onto a function, in both cases both values are popped and the function is applied to the value.

Some functions, including all native functions, are specifically defined to only interact with one value at a time. For binary operations, like addition, this means the function can be partially applied: `1 + ` produces a closure that will add `1` to a number.

Finally, what application means varies a little based on the type of function involved:

 * For native functions, we call a function pointer stored in the function value.

 * For user-defined functions, we set the instruction pointer (of smålang, not the system) to the start of a string stored in the function value.

### Built-in Functions

Smålang includes a small set of built-in operations for performing arithmetic and defining procedures.

|Name|Prototype|Effect|
|---|---|---|
| `inc` | `x -> x` | Adds 1 to a number. |
| `dup` | `x -> x x` | Duplicates the top stack element. |
| `+` | `x -> x -> x` | Adds two numbers together. |
| `-` | `x -> x -> x` | Subtracts the first number from the second. |
| `*` | `x -> x -> x` | Multiplies two numbers together. |
| `/` | `x -> x -> x` | Divides the first number by the second. |
| `<` `<=` `>` `>=` `==` `!=` | `x -> x -> x` | Compares two numbers. |
| `and` `or` | `x -> x -> x` | Performs binary logic on two booleans. |
| `not` | `x -> x` | Negates a single boolean. |
| `=` | `x -> x -> ()` | Binds a variable name to a value. |
| `fn` | `x -> x -> ()` | Defines a function with a provided name and body. |
| `if` | `x -> x -> x -> x` | Evaluates one of its two branches based on a condition. |
| `putv` | `x -> ()` | Prints a value to standard output. |

### Last Remarks

I basically ran out of time working on the documentation, so please look at some of the examples (`example/`) for more!