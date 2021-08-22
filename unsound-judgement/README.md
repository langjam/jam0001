# nanodijkstra

"I don't know how many of you have ever met Dijkstra,
but you probably know that arrogance in computer science
is measured in nano-Dijkstras."
\- Alan Kay

nanodijkstra is a programming language that bridges the gap between formal
theorem provers, and regular programming languages - first class preconditions
and postconditions one can refer to, and which one must prove; however, the
proofs that one writes are informal, written in plain language.
This gives a nice balance of "proving one's program correct",
while also skipping out on the complexity of a true formal theorem prover
(and also the complexity and difficulty of writing all one's preconditions
in a formal language).

Examples are located in `examples`.

## Using the compiler

nanodijkstra is very easy to use.

### Build

```sh
$ cargo build
```

### Run

```sh
$ cargo run file.nd
```

## How do proofs work?

There are two kinds of "propositions" - preconditions, and postconditions.

Preconditions must be proved at the site of a call.
Postconditions must be proved in each control flow.

The way one proves preconditions is as follows:

Let `f` be a function with a precondition:

```nd
fn f() [
  `p1`
] -> () {
  ()
}
```

Then, in order to call `f`, one must prove `` `p1` ``:

```nd
fn main() -> () {
  f() [proof `p1` = `this is why p1 is true`]
}
```

One can also place the proof out of line:

```nd
fn main() -> () {
  proof @p1: `p1` = `this is why p1 is true`;
  f() [@p1]
}
```

Postconditions are similar, except that they _must_ be given names:

```nd
fn f() -> () [
  @tuple_is_tuple: `$return = ()`;
] {
  proof @tuple_is_tuple = `obvious`;
  ()
}
```

and they must be proved in each control flow path.

When calling a function, in order to access postconditions, one can write:

```nd
fn main() -> () {
  f() [-> @tuple_is_tuple: `$return = ()`]
}
```

or

```nd
fn main() -> () {
  f() [-> @tuple_is_tuple];
  known @tuple_is_tuple: `$return = ()`;
  ()
}
```

in order to break your code when a postcondition changes,
when relying on a postcondition one should add these postcondition annotations;
then, if the callee changes their postcondition, you'll find out!

### What is the `$`?

The `$` is not special to the compiler; it's just a human convention.

- `$return` means "the return value of the function call"
- `$name` means "the value of the `name` parameter of the function"

## Notes

1. Error handling is very much Not Good. Errors are not pretty.
2. This language is expression oriented and immutable.
3. Proofs and propositions are not parsed; they are matched by string-equality.
