## Zac Programming Language
An interactive scripting language where you can read and modify code comments as if they were regular strings. Add and view text-based visualizations and debugging information inside your source code file.

### [Try it in your browser right away](https://sumeet.github.io/Zac/)
The experience is better locally though, read on!

![GoL](.README_assets/GoL.gif)

Since this is an interactive editor, the best examples are moving use cases.

#### Built-in Help
![help](.README_assets/help.gif)

#### Workbook style examples that don't go out of sync
![fib](.README_assets/fib.gif)

### Cargo Install Instructions
Make sure you have the [Cargo package manager](https://crates.io/) with a recent nightly Rust, and from inside this
directory:
```console 
cargo install --path .
```

### How To Run
As an example, open [examples/hello.zac](examples/hello.zac) in your editor. Then, run (preferably through your editor)

```console
zac examples/hello.zac
```

(or if you're a Rust coder)

```console
cargo build --release
target/release/zac examples/hello.zac
```

![hello](.README_assets/hello.gif)

### More Examples
- [GoL.zac](examples/GoL.zac)
- [fib.zac](examples/fib.zac)
- [help.zac](examples/help.zac)
- [the in-browser version](https://sumeet.github.io/Zac/)

#### It's Better With Syntax Highlighting
If you're using Vim, there's a [syntax file](syntax_highlighting/) in the repo. Put this in your `~/.vim/syntax` directory, or `~/.config/nvim/syntax` if you're using Neovim, and follow the instructions at the top of the file.

#### Interactive Editing
It's highly recommended that you save the file from inside of your editor instead of changing windows. Zac is meant to be used like Gofmt or Rustfmt, run every time you save your file.

In Vim, you can use the following command to save your file:
```vim
:map \t :w\|:!zac %<CR>:e<CR>
```

### Zac Language Overview

You'll have a better time getting a feel of how the language works from looking at and running examples than from reading detailed documentation. Other than comment modification, Zac is a familiar-looking scripting language.

Comments come in two flavors, anonymous and named.

```js
// This is an anonymous comment because it doesn't have a #identifier as
// the first line.
//  
// This cannot be referenced or modified from within the code.

let #changeme = // some string

// #changeme
// This is a comment with the name #changeme.
// 
// In this program, it is modified twice, once above and once below
// using the `let` expression.
//
// After running this program, this comment will instead contain:
// // some string another string

let another_string = // another string
let #changeme = cat(#changeme, chr(32), another_string)

// Named comments can be changed before and/or after they appear in the source code. Zac will throw
// an error if there are two comments in a program with the same name.
```

As of now, there is no specific syntax for string literals, if you need a string literal, you can make a comment.

### Status
This is a proof-of-concept I made in the first [Lang Jam](langjam/langjam), a 2-day competition to design a programming language around the theme `first-class comments`.

Submitted to the contest under the team name SOLDIER.

<img src=".README_assets/firstclass.png" height="100px">
