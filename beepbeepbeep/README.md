# `// TODO: fix`

The language where endless procrastination of your programmer responsibilities is useful!

## Overview

`// TODO: fix` is a programming language by [RocketRace](https://github.com/RocketRace) for the [first Langjam event](https://github.com/langjam/jam0001), consisting of only naturalistic "TODO" comments.
These comments define the behavior of a `// TODO: fix` program entirely. Anything
outside these comments will be ignored, so this language is perfetly suited for
embedding programs into "normal" code! Your coworkers might be concerned, though.

Here's an example program that outputs "Hello, world!":
```rust
// TODO: someone on the internet told me "Hello, world!" is a good idea
// TODO: somehow fix this cursed code
```

But it could just as well be written like:
```rust
use std::io::{stdout, Write};
// TODO: someone on the internet told me "Hello, world!" is a good idea
fn hello() {
    let chars = &[
        '\n', '!', 'd', 'l', 'r', 'o', 'w', ' ', ',', 'o', 'l', 'l', 'e', 'H'
    ];
    let mut out = stdout();
    // TODO: somehow fix this cursed code
    for c in chars.iter().rev() {
        let mut buf = vec![0; 4];
        c.encode_utf8(&mut buf).unwrap();
        out.write(&buf).unwrap();
    }
    out.flush.expect("unwrap");
}
```
Feeling productive yet?

## Running

This interpreter is written in Rust. Any recent version of the compiler should be fine; 
there's nothing particularly fancy in the requirements.

For a quick* run, `cargo run [path to your file]` or `cargo run --release -- [path to your file]` will do.

### Note on errors

The errors are, uh bad. Really bad. Just, uh, try not to write invalid programs, please?

## Example programs

A list of example programs are shown in the `examples` folder.

## Execution model

A `// TODO: fix` program is a sequence of operations and function definitions. Functions
contain operations and are delimited, forming clear scopes.

Execution is linear, starting from the top and going down. When encountering a function, the program counter skips until its end. When calling a function, the program counter jumps to its start, and upon completion continues after the last executed line. (Really, these "functions" are more like labels without fallthrough.) Some operations may also cause the program counter to skip forward an instruction.

Everything in `// TODO: fix` is a value -- a 64-bit unsigned integer. These are accessed through a global stack, as well as a variable mapping from names to values:

* The stack is at the center of most operations. It's unchanged when changing scopes.
* The variable mapping is scope-dependent. Each function scope has its own set of variables accessible through it. Variables must also consist only of the ASCII alphabet (a-z, A-Z).

Functions can take zero or more arguments. These are passed through the stack. How these arguments are pushed to the stack is the responsibility of the caller, and unenforced by the language. In certain cases, a function may be called with named parameters. In this situation, where a function is called with e.g. the paramaters `Foo`, `Bar`, and `Baz`, the values corresponding to `Baz`, `Bar`, and `Foo` will be pushed to the stack in that order.

There is also a condition flag, which is read from and written to by certain operations. It isn't cleared, ever. Do it yourself! >:(

## Operations

*`// TODO: fix` has many different ways to spell out an operation. For brevity's sake, only samples will be shown here, rest listed in `DOCUMENTATION.md`. Please note that the file is auto-generated from the grammar, and may thus be partially incorrect or incomplete.*

Each operation below is shown alongside an example of how it's used. **Do note that you have a *lot* of freedom in how you express each operation!**

### Increment: `// TODO: fix off-by-one error`

Pops `x` from the stack, pushes `x + 1` to the stack.

### IncrementOne: `// TODO: handle off-by-one with Foo here`

Sets `Foo` to `Foo + 1`.

### SimpleAdd: `// TODO: debug integer overflow with addition`

Pops `x` from the stack, pops `y` from the stack, pushes `x + y` to the stack.

### AddOne: `// TODO: fix overflow when adding by Foo`

Pops `x` from the stack, sets `Foo` to `x + Foo`.

### AddTwo: `// TODO: debug overflowing when adding Foo and Bar`

Pushes `Foo + Bar` to the stack.

### SimpleMul: `// TODO: fix integer overflow multiplying`

Pops `x` from the stack, pops `y` from the stack, pushes `x * y` to the stack.

### MulOne: `// TODO: handle overflow when multiplying with Foo`

Pops `x` from the stack, sets `Foo` to `x * Foo`.

### MulTwo: `// TODO: fix overflow when multiplying with Foo and Bar`

Pushes `Foo * Bar` to the stack.

### SimplePow: `// TODO: handle overflow with exponent`

Pops `x` from the stack, pops `y` from the stack, pushes `x ** y` to the stack.

### PowOne: `// TODO: fix overflowing taking the power to Foo here`

Pops `x` from the stack, sets `Foo` to `x ** Foo`.

### PowTwo: `// TODO: debug integer overflow with Foo to Bar power`

Pushes `Foo ** Bar` to the stack.

### SimpleSub: `// TODO: fix underflow with minus`

Pops `x` from the stack, pops `y` from the stack, pushes `x - y` to the stack.

### SubOne: `// TODO: fix integer underflow subtracting by Foo`

Pops `x` from the stack, sets `Foo` to `x - Foo`.

### SubTwo: `// TODO: handle underflowing when subtracting Foo with Bar`

Pushes `Foo - Bar` to the stack.

### SimpleDiv: `// TODO: fix divide-by-zero panic here`

Pops `x` from the stack, pops `y` from the stack, pushes `x / y` to the stack.

### DivOne: `// TODO: debug dividing by zero with Foo`

Pops `x` from the stack, sets `Foo` to `x / Foo`.

### DivTwo: `// TODO: fix divide-by-zero error with Foo and Bar`

Pushes `Foo / Bar` to the stack.

### SimpleMod: `// TODO: handle modulo by zero`

Pops `x` from the stack, pops `y` from the stack, pushes `x % y` to the stack.

### ModOne: `// TODO: fix modulus-by-zero crash with Foo`

Pops `x` from the stack, sets `Foo` to `x % Foo`.

### ModTwo: `// TODO: fix modulo-zero of Foo and Bar here`

Pushes `Foo % Bar` to the stack.

### PlainFunction: `// TODO: prevent Foo being called in malformed contexts`

Defines a function `Foo` that can be called using the Call or CallWithArgs operation.

### FunctionWithArgs: `// TODO: document the 16th argument to Foo`

Identical to PlainFunction, but has some flavor. Use it if you like :)

### EndFunctionBlock: `// TODO: fix the formatting here`

Ends a function block. Executing this returns you back to where you called the function.

### PlainFunctionCall: `// TODO: ensure Foo is never called`

Starts executing the function `Foo`.

### FunctionCallWithArgs: `// TODO: make sure calling Foo with Bar, Baz, doesn't cause a crash`

Pushes `Baz`, `Bar` to the stack, then starts executinf the function `Foo`.

### SkipOne: `// TODO: do some proper error handling`

Skip the next operation if the condition flag is set.

### Equality: `// TODO: handle invalid states here`

Pop `x` from the stack, pop `y` from the stack, set the condition flag to `x == y`.

### Nonzeroness: `// TODO: checking for null here`

Pop `x` from the stack, set the condition flag to `x != 0`.

### Dup: `// TODO: clear out a lot of this bloat`

Pop `x` from the stack, push `x` to the stack, push `x` to the stack.

### Pop: `// TODO: fix unnecessary dead code around Foo`

Pop `x` to the stack, set `Foo` to `x`.

### Push: `// TODO: fix all these stupid errors with Foo`

Push `Foo` to the stack.

### Number: `// TODO: fix 5 more errors`

Push `5` (the specified number) to the stack.

### PushStr: `// TODO: according to my coworkers, i should "take a shower"`

Push the unicode codepoints of `"take a shower"` (the specified string) to the stack in reverse order, push the length of the string to the stack.

### SerializeNum: `// TODO: fix this type mismatch here`

Pop `x` from the stack, push the base-10 string representation of `x` to the stack as with PushStr.

### StrOutput: `// TODO: attempt to fix this mildly cursed code`

Pop `x` from the stack, pop `x` numbers from the stack and write them to standard output.

### StrInput: `// TODO: perhaps i should read the docs`

Read one line of input from stardard input, and push that string to the stack as with PushStr.

### Halt: `// TODO: just give up already`

Exit the program immediately.
