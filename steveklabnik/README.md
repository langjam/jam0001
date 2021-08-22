 # Tom's Obvious, Minimal Lisp (by Steve)

Code is data. Data is code. What's a popular data format? TOML. So why can't
TOML be code?

`tomlisp` is a lisp, but with TOML syntax. Here is "Hello world" im tomlisp:

```toml
# hello, world!
main = "hello, world!"
```

The `main` key's value is your program, which will be executed, and the final
value printed to the screen before the interpreter exits.

This jam's theme is "first-class comments." When asked to elaborate, jt said:

> there are many ways to interpret "first-class", all of them are valid

I've been on an airplane before. I know what "first class" means. It means that
they're at the front, and there's a limited amount of space. So in tomlisp,
your TOML files must start with a comment. This is the only place a comment is
allowed to live. And you get 20 characters to work with. If you use more
than that, your program will be rejected.

## Compiling and running

It's a Rust project, so install Rust. Then use `cargo run`:

```console
$ cargo run samples\langauge2.toml
```

The `samples` directory contains a few sample programs.

## Stuff that's supported

* All TOML values
* `if`: takes a test, the true value, and the false value
* `begin`: evalutes each value in turn
* `define`: saves a value in the environment
* `lambda`: creates a closure
* `*` and `+`: multiplies or adds all numbers together
* `pi`: you know what this is already

## Why aren't other things supported?

Because this is a 48 hour jam and I am getting old and tired. You don't need
to divide anyway.

## Known bugs

Because TOML doesn't support bare words, symbols are strings, and strings are
symbols. Currently, this means that when evaluating a string, it will attempt
to resolve it as a symbol first, and if that fails, resolves it as a literal
string instead. This means you can't have string literals with the same name
as anything you've defined in the environment. This could be resolved if I cared
enough to fix it, but this is just a fun jam. I'd probably make strings that
start with $ symbols or something.

## Acknowledgements

Thank you to everyone who came by [my
stream](https://www.youtube.com/watch?v=bze2FAUu32Q). The only idea I had coming
into the jam was "homoiconic TOML," and the stream was invaluable in making me
realize that I was just writing a Lisp for real rather than something lisp-ish
and so I should just simplify what I was doing into a Lisp. Lots of great
brainstorming and conversation in there. I wish I had the energy to do it the
whole time.

Whenever I need to knock out a little Lisp, I turn to the classic article by
Peter Norvig, ["(How to Write a (Lisp) Interpreter (in
Python))"](http://norvig.com/lispy.html). I once again turned to it for this
project. This is why pi is in there, for example.

Additionally, ["Risp (in (Rust) (Lisp))"](https://stopa.io/post/222) by Stepan
Parunashvili was useful. This is an implementation of Norvig's Lisp in Rust, and
our implementations end up being pretty similar, first of all because we're
working on the same source material, but also because I shamelessly looked at
his code when I felt stuck. Any good things that look similar to his stuff is
due to him, not me :) One extremely notable difference is that he does real
error handling in his code, while I just panic when I hit a problem.
