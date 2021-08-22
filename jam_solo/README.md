# Zash
## Where Comments are First-Class Citizens!

## Source Code
zash is built with `Zig` the most awesome programming language. You can find the
source code in the `zash/src` sub-directory. The `zash` sub-directory per se is 
the Zig project directory, where you can build an executable binary via the 
[Zig compiler](https://ziglang.org/download/).

## How to Build from Source
Download the `master` development Zig compiler from https://ziglang.org/download/ .
Extract the archive and add the path to where your OS looks for executables. You
should be able to run `zig version` from the command line now.

Now in the `zash` sub-directory, you can run `zig build` to obtain a debug binary
in `zash/zig-out`. For a release build, you can run `zig build -Drelease-fast`.
Binaries all always placed in `zash/zig-out` when using `zig build`. 

## Running the Sample
A sample zash source code file is provided as `example.zash`. It demonstrates how the
langugae treats comments as first-class citizens. Namely it shows:

1. Comments start with `#:` and can either end at the next newline or at a closing `:#`.
2. Comments can be nested.
3. Comments can be stored as values in variables.
3. Comments can be passed as arguments to functions, so functions can have comments as parameters.
4. Comments can be returned from functions.

To run the sample, build your binary executable via `zig build` as explained above, and provide 
hte zash source code file as a parameter. For example:

```
$ ./zig-out/zash ../example.zash
```

## Langugae Features
The language is quite limited, but does have some basic features:

1. Only integer numbers are allowed. We all know that 
[God created the integers, all else is the work of man.](https://mathshistory.st-andrews.ac.uk/Biographies/Kronecker/#:~:text=God%20created%20the%20integers%2C%20all,a%20finite%20number%20of%20operations.&text=Another%20consequence%20of%20his%20philosophy,transcendental%20numbers%20could%20not%20exist.) :D
2. Arithmetic operations on integers (`+`, `-`, `*`, `/`).
3. Comment concatenation with `++`.
4. Function definition and calling.
5. Variable definition with the infix `:=` operator.
6. Variable assignment with the `=` operator.
7. Boolean literals `true` and `false`.
8. Integer and Comment equality / inequality via `==` and `!=`.

The langugae is mostly whitespace agnostic, only using newlines as relevant 
breakpoints for expressions in some contexts. No semicolons at line endings
allowed, but could be made optional without much fuss.

Here's the example file for reference:

```
#: Welcome to the zash Programming Language!
#: Where comments are First-Class Citizens.

#: Comments extend to the end of the line...
y := #: ...can be assigned to a variable. :#

x := 2 * 4 #: ...can be embedded! :# / 2

y = #: ...can be #: nested :# too. :#

#: This function concatenates comments.
#: So it receives comments as args and 
#: returns a coment. First-class baby!
fn comment_concat(a, b) {
    a ++ b
}

#: Here we pass a comment literal(?) and
#: a variable containing a comment as a value.
comment_concat(#: Pre: :#, y)
```
