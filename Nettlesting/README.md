# Nettlesting

A language written in C using the Lemon parser generator. Or, it would be one, if I had finished it.
As it stands there's only a tokenizer and a basic grammar file, which can parse the example code in `examples/bottlesofbeer.ntl`.

## Syntax

I understand first-class comments to mean that comments are an integral part of the language. Data definition and data description should happen in the same place.
In Nettlesting, a program is made up of two types of blocks - comment blocks  `{ ... }` and code blocks `[ ... ]`.

### Types

Nettlesting only has a few basic types:

- number - hex, octal, and decimal notation is supported
- string - text surrounded by either double `"` or single `'` quotes
- comment block

### Comment blocks

Comment blocks provide data storage and insight.
Storing data in a comment block is achieved through attributes of the form `#<name>: <expression>`, followed by a semicolon.
Subsequently, text written inside a comment block may not start with the `#` character, however it may contain it later on.
Additionally, text in a comment block may not contain the characters `{` or `}` - the closing bracket will mark the end of a comment, while
the starting bracket will produce an error.

A comment block with a `name` attribute may be bound to and used in code blocks using the syntax `{ ... } <= ...` - this must follow a newline inside
a codeblock, but not outside of one. An expression or a code block itself may be bound to a comment.
In addition, a comment bound to a code block may define the `bindings` attribute - a comment block defining what parameters the code block can take.
These may be used as regular variables inside of the code block.


### Code blocks

Code blocks provide a way to transform the data as defined in local and bound comment blocks. They consist of statements, interspersed with local comments, and must always be followed by a semicolon, even in the case of if and while statements.
There's a few different statements available for a code block:

- `let name := <expression>`
- `if <expression> [ ...code... ]`
- `if <expression> [ ...code... ] elseif []` - else is not supported.
- `while <expression> [ ...code... ]` - must be bound to a comment block with a `name` attribute for `break` and `continue` statements to work.
- `break name`
- `continue name`

Code blocks bound to comment blocks may be called using the syntax forms:

- `#name;` - for calling code with no variables.
- `#name <= { ... };` - for calling code with named variables.

## Default functions - unimplemented

- `printf`
- `concat`

## Prerequisites

- Lemon parser generator
- Ninja build system

## Compilation

`cd Nettlesting && ninja`

## Usage

`nsting [source]`

Do not use. Leaks memory everywhere and segfaults in the middle.