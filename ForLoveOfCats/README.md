# StackTrack

## Build

Uses standard Rust stable toolchain, developed and tested with 1.54.0. A simple
`cargo run` will build the interpreter and run `program.stack` in this directory.

Pulls down no dependencies from crates.io

## Rapid fire description

 - Strongly dynamic typed
 - Pass by value
 - Scope based memory management
 - S-Expression syntax
 - Tree walking interpreter
 - Fully expression based, everything evaluates to a value
 - Supported types are None, Bool, I64, List, and String
 - Integer math, boolean logic, equality, and basic List/String ops built in
 - Dynamically scoped variables
 - All user-defined functions are first class lambdas
 - Comment stack machine (see more below)
 - Full repl on panic (both manual and interpreter panic)

## Detailed description

This description will be a bit indulgent with thoughts, comments, and notes.

I knew going in that I would end up using s-expressions for simplicity of
tokenizing and parsing. As such a tree walker is an obvious next choice. By
going with pass-by-value I was able to avoid issues with shared mutability and
the language needs no memory management apart from things going out of scope.
However that means that there is no way to pass mutable references around which
would limit potential things like chaining iteration but that's okay for the
scope of the jam.

First class comments was a very difficult prompt to work with. The trickiness in
my eyes is how first-class is defined. Ideally the prompt means that the code
can perform runtime manipulations on the comments, like pass them around or
modify their contents. The problem with this is that everything one could do in
this area can already be done in existing languages with strings. A runtime
comment is just a string. Want to insert extra data into it, that's a string.
Want to pass an explanation of error context to a function, again a string.

Because of this it took quite a while to land on something which I feel both
satisfies the jam prompt while also being useful. There is some leniency with
how to interpret the prompt I understand which should result in some cool
entries but I wanted to remain as true to "first-class" as usually defined.

Usually in my code comments serve to give some context or relevant information
which is not already obvious from the code itself. One common subset of this in
the wild is to describe error states. Why and how an error might occur or the
assumptions that some code is making. I chose to focus on this specific usage
case.

This language has what is easiest to describe as a comment stack machine. As
comments are encountered at runtime, the interpreter maintains the stack of
scopes and their comments. A scope's comments only contains the comments up to
the point of execution in that scope and is discarded when going "out of scope".
Furthermore comments may contain arbitrary runtime data and have the same
"syntax" as the print and panic functions.

Whenever a manual panic or runtime error is encountered, the user is dropped
into a full repl. From there they may inspect and modify variables, call
functions, and display the current comment stack (via `comments`). At any point
in the repl the user may also return control flow (via `return`) or terminate
the application (via `exit`).

This lends itself to be a surprisingly powerful debugging tool where comments
can be littered around to provide context and also log potentially important
information. This log is not visible unless explicitly requested by the user in
the case of an error and only contains the specifically relevant entries.

Try playing around with the provided `program.stack` which implements a reverse
polish notation interpreter. Try passing invalid arguments, poke at variables,
and inspect the comment stack.

Given more time I would have liked to spend more time on cleaning up error
messaging and internal error handling. However I am very happy with how this
turned out and I think that the comment stack can be an interesting mechanism to
play with.
