# Langjam #0001

**Theme:** first-class comments

---

## Concept

Conceptually, the main ideas for this language were

* making comments available at runtime, as metadata attached to various elements of the code
* allowing modification, addition and removal of comments at runtime
* allowing comments to contain expressions, e.g. `greaterThan(_, 5)`, which will be parsed in the same way as the rest of the language
* allowing execution of comments based on the comment type, e.g. an `assert` comment on a variable, which would be executed every time the variable changes.

Unfortunately there wasn't enough time to implement most of this functionality, but this is what it *might* have looked like:

```
fn area(
  <assert: greaterThan(a, 0)>
  a,
  <assert: greaterThan(b, 0)>
  b
) {
  multiply(a, b)
}

print(area(5, 10))
print(area(-1, 10))
```

When running this example, the expected result would have been:

```
50
Error: Assertion on `a` – greaterThan(a, 0) – failed. `a` was -1.
```

## What was actually achieved

The language supports attaching comments to identifiers and accessing them at runtime:

```
fn i_like(
    <indicates which thing you like> the_thing,
) {
    let x = "I like "
    print(the_thing.$comment)
    print(length(the_thing.$comment))
    concat(x, the_thing)
}

print(i_like("turtles"))
```

This snippet should result in

```
# => String("indicates which thing you like")
# => Integer(30)
# => String("I like turtles")
```

## Building

### Requirements

* `cargo` and Rust need to be installed
  * Tested with `rustc 1.54.0 (a178d0322 2021-07-26)`

### Building

`cargo build` will build the `target/debug/langjam0001` binary.

### Testing

Run `./go test` to see if all examples in the `test/` directory can be run successfully. Doesn't currently test whether the output is correct 😶


## Properties of the language

In no particular order

* Comments can be specified in pointy brackets `<the comment>`, and can be placed before statements and expressions (the idea is "everywhere")
* The following built-in functions exist: `print`, `plus`, `multiply`, `length`, `concat`.
* There are no operators
* Conditions also did not make the cut 😅
* The return value of a function is the return value of its last expression
