# Comment-errors

This project is not a full language, but rather adds *first-class comments* to an existing language: **Rust**.

It is a [proc-macro](https://doc.rust-lang.org/reference/procedural-macros.html)
that hijacks doc comment syntax inside function definition to use them as
templates for nicer error-reporting.

The `proc-macro` source is in `comment-errors`. `comment-errors-test` contains a few examples.

## Running the example

```bash
cargo run -p comment-errors-test
```

Note, on `NixOS` I was having issues getting the `proc-macro` to compile properly and ended up doing:

```bash
cargo +nightly run -Ztarget-applies-to-host -p comment-errors-test
```
