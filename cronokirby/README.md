# Wahlbergdown

This is Wahlbergdown, a programmable version of Markdown, submitted to
[LangJam 0001](https://github.com/langjam/jam0001).

This README file is actually a Wahlbergdown program!

Try running:

```
cargo run --release -- README.md
```

The next part should be "42": ``(+ 40 2)``.

## Building

```
cargo build --release
```

## Usage

You can do:

```
cargo run -- --help
```

for help with commands.

For running files, you do:

```
cargo run --release -- run <file>
```

(I recommend running in release, for performance)

You can also see the output of the lexer, or the parser:

```
cargo run -- lex <file>
cargo run -- parse <file>
```

You can also build the project, and put the `wahlbergdown`
executable in your PATH, by running:

```
cargo install --path .
```

## Language

From this part onwards, I'd recommend opening this markdown file in a text editor,
so you can see the actual syntax. Otherwise, these explanations are going to
seem very confusing.

The language takes in a markdown file as input, and spits out a markdown
file as output.

You can interpolate using two ticks:

``3`` ``4`` ``(+ 3 4)``

Basic arithmetic expressions are available:

``(+ 1 1 2)`` ``(- 3 1 1)`` ``(* 2 3 4)`` ``(/ 40 2 2)`` 

Logical expressions are available as well:

``(and 1 0)`` ``(and 1 1)`` ``(or 1 0)`` ``(or 0 0)``

``(not 1)`` ``(not 0)``

You also have if expressions:

``(if 1 2 3)`` ``(if 0 2 3)``

You can also define values in comments:
<!-- x is 32 -->
<!-- y is 44 -->
``x`` ``y`` ``(+ x y)``

You can also define new functions:

<!--(add x y) is (+ x y)-->
``(add 2 3)``

These can be recursive:

<!--
(add x y) is
(if (= x 0)
  y
  (+ 1 (add (- x 1) y)))
-->
``(add 2 3)``

Tail-recursion is detected, to avoid stack overflows:

<!--
(add-tr x y) is
(if (= x 0)
  y
  (add-tr (- x 1) (+ y 1)))
-->
``(add-tr 1000000 2)``

If you try that function with `add`, it will blow up :p

And, that's about it. You can also check the `examples` directory for more.
