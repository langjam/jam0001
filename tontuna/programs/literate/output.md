# Literate mode!

This is a program written in literate style.

While normally we write programs as "code with added comments", Tontuna allows
us to flip this the other way around - programs can be written as "comments with
added code". While in the regular mode all text is by default assumed to be
code and comments need to be prefixed with '#', in literate mode text is a
comment by default and instead we prefix code with '>' (just like we do when we
want to embed code inside a comment).

As programs still have the ability to introspect their own source, we will build
a little demo - this program will format its own source as a nice little
markdown document.

For starters, we have our main function that will retrieve the code and invoke
formatting implementation. And also print out a header because I felt like
including something extra in here. Here you go:


```rust
fn main() {
    println("# Literate mode!");
    println();
    emit_markdown(program_source());
}
```

For simplicity we will have very simple and minimal formatting functionality.
All plain text (which is really comments under the hood) will be written out as
text directly to markdown, and code will be emitted as code blocks. Here's a
function that iterates over the elements in the source and dispatches to
appropriate emitting functions:


```rust
fn emit_markdown(code) {
    for item in code {
        if let comment: Comment = item {
            emit_comment(comment);
        } else if let code: Code = item {
            emit_code(code);
        }
    }
}
```

Now we need the two functions for formatting code and comments. For comments
there's not much to do, we just print out the text:


```rust
fn emit_comment(comment) {
    println(comment.text);
}
```

For code the situation is a bit more complicated. If we get the text for the
code block every line will be prepended with ">". And of course to make our
lives harder, the first line won't be.

Fixing up the code properly is very difficult, but the problem here is that the
code introspection api sucks (if only I had more time to make it better). We'll
do the bare minimum here: we wil just strip "> " prefix from every line (first
line won't have the prefix and remain unchanged).

Also we'll pretend that this is Rust code because the languages are
syntactically similar enough to give us decent syntax highlighting.


```rust
fn emit_code(code) {
    println("```rust");
    let lines = split_lines(code.text);
    for line in lines {
        let stripped = strip_prefix(line, "> ");
        println(stripped);
    }
    println("```");
}
```

Here's the first helper function, that splits a string into separate lines:


```rust
fn split_lines(text) {
    let lines = List();
    let last_start = 0;
    let idx = 0;
    while idx < text.len {
        if text.get(idx) == "\n" {
            let line = text.substring(last_start, idx - last_start);
            lines.push(line);
            last_start = idx + 1;
        }
        idx = idx + 1;
    }
    if last_start < idx {
        let line = text.substring(last_start, text.len - last_start);
        lines.push(line);
    }
    return lines;
}
```

And the second one, to strip the prefix if the line starts with it:


```rust
fn strip_prefix(line, prefix) {
    if line.len < prefix.len {
        return line;
    }
    let actual_prefix = line.substring(0, prefix.len);
    if prefix == actual_prefix {
        return line.substring(prefix.len, line.len - prefix.len);
    } else {
        return line;
    }
}
```

And that's it! You can run this file directly and it will print out a markdown
version of itself. Of course, the markdown version is not runnable, but you can
render that one and it will probably be more pleasant to read that this
monospaced wall of text.

And let's not forget to actually run this:


```rust
main();
```
