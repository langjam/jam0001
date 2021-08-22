# Gold

The Gold Rush began in 1948. This programming language is developed in 48-hours. Coincidence, I
think not!

## What is Gold?

Gold is a shiny metal that people really like for some reason. Gold is also a programming language
which is new and shiny (like the metal) and solves a crucial problem for industry veterans and CS
Students alike.

Have you stubbed out a function, commented all of its parameters and outputs like a good little
programmer, and immediately realized that your comments duplicated the function stub? With Gold, you
will never have this experience again.

Gold brilliantly solves this problem by eliminating most of the function declaration entirely. With
Gold, function declarations occur in the comments. By creating a grammar from a subset of the
english language.

Gold is JIT compiled using Cranelift, so it should be fairly fast. Gold also has an in-progress
standard library written in Rust, currently only consisting of functions for printing strings.

## Installation
You will need the Rust toolchains installed.

Use git to clone the repository.

Then, from the repository root, do the following:
```bash
cargo install --path ./gold
```

## Examples

### Looping

```text
// foo is a function.
// Params:
// 'a' is of type Int.
// 'b' is of type Int.
// Returns: Int
fn {
  var cool = 10;
  while cool > 0 {
    cool = cool - 1;
  }
}
```

### Printing

```
// hello is a function.
// Params:
// Returns: Void
fn {
  print("Hello, world");
}
```

## Project Status
Compiler is unfinished.
Hello world works at least.
Jit is wired up, need to resolve a few parser errors.