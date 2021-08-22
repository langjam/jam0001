# Liner

Liner is a simple programming language where comments enable metaprogramming.
In Liner, any comment that contains valid Liner code can modify the program by manipulating the AST or outputting code.

## Build instructions

1. Obtain a nightly Rust compiler using [`rustup.rs`](https://rustup.rs/). Note: if your default rustc is stable, prefix each cargo command with `+nightly` or [switch to nightly by default](https://stackoverflow.com/a/58228429).
2. Build the project and run the tests with `cargo test`.
3. Read the next section and run the sample programs using `cargo run --release -- <file.ln>`. For example, to run readme.lm, execute:

```bash
$ cargo run --release -- examples/readme.ln
x is less than y
{ 7: "seven", bool: true, cmp: <fn: lambda@310:475>, nil: null, number: 42, string: "a string" }
5
9
```

4. (optional) Alternatively, compile with debug assertion to see comment parsing errors:

```bash
$ cargo run -- examples/readme.ln
[Parser] Failed to evaluate a comment: error at 2:19: expected one of "!=", "&&", "*", "+", "-", "/", ";", "<", "<=", "=", "==", ">", ">=", "[", "||", [' ' | '\t' | '\n' | '\r'], [' ' | '\t']
<--snip-->
```

5. (optional) To display the initial AST of the program, use the `-a` flag:

```bash
$ cargo run -- examples/readme.ln -a
Ast
╰─statements=↓
  ├─LineComment
  │ ├─body: CommentBody::Text
  │ │ ╰─text: "Liner has objects (essentially hashmaps), functions (named and anonymous), numbers, strings, booleans, and null.\nIn terms of control flow, there are if-else, while, break/continue, and return statements."
  │ ╰─stmt: StmtKind::LetDecl
  │   ├─name: "object"
  │   ╰─initializer: ExprKind::ObjectLiteral
  │     ╰─field0=↓
...
<--snip-->
```

## Syntax and Semantics

Liner's syntax is very much Rust-inspired while its semantics closely resemble Lua, although with a few minor differences. The following snippet demonstrates the available data types and syntactic constructs:

```rust
// File: examples/readme.ln

// Liner has objects (essentially hashmaps), functions (named and anonymous), numbers, strings, booleans, and null.
// In terms of control flow, there are if-else, while, break/continue, and return statements.
let object = {
    number: 42,
    string: 'a' + " string",
    bool: true,
    nil: null,
    cmp: fn (x, y) {
        if x == y { return "x is the same as y"; }
        else if x < y { return "x is less than y"; }
        else { return "y is less than x"; }
    }
};
// Assign dynamic properties with the bracket syntax
object[7] = "seven";
// And access fields with the dot syntax
print object.cmp(2, 3);

// Almost everything can be pretty-printed.
print object;

while object.number < 50 {
    object.number = object.number + 1;
    break;
}

// Functions can contain spaces in their names
fn display sum(x, y) { print x + y; }

// and be called either normally
display sum(3, 2);

// or with the `please` and `do` statements
please display sum of 4, 5.
```

## First-class comments

Comments in Liner are meant to do metaprogramming. A Liner comment may modify the program in two different ways:

1. By printing code inside a comment.
2. By directly modifying the AST object of the next statement.

Let's look at both ways:

```rust
// File: examples/comment-time-execution.ln

// Note: At the moment, the entire comment group must consist of valid Liner code, so
// double comments have to be employed if additional annotations are desired.

// // Way #1: print the desired code
// // The comment below will cause the `null;` statement to be replaced with a print.
// print "print 'Hello, World!';";
null;

// // Way #2: modify the AST
// AST.__stmt__ = "print 'changing the AST';";
null;

// A function defined during comment-time execution will be available to all following comments,
// but not the runtime objects:

// fn display ast() {
//     dbg(AST.tree);
// }

print "runtime output";

// // Now we can use this function to print a statement's AST like this:
// please display ast
let tau = 3.151592 * 2;
print tau; // this statement will not be affected

// // It is possible to selectively modify parts of the ast:
// AST.initializer.left.name = "tau";
let pi = X / 2;

print pi;


// Printing the AST at comment-time was neat, but what if we want to do this at runtime?
// Easy, we just need to insert it into the output stream, while preserving the existing content:

// fn display ast at runtime () {
//     AST.__stmt__ = fmt("{ print '{}';\n {} }", AST.tree, AST.code);
//     AST.unscoped = true; // this disables the block scoping introduced on the previous line
// }

// please display ast at runtime
let receipt = { subtotal: 146.6, tax: 0.13 };
print receipt;
```
