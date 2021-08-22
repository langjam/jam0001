# Coqui

## Elevator Pitch

When programming, we typically use comments to provie context to, express technique of, or provide supplemental information about our programs. We as programmers use our code as the primary description of "what" is being done, and while code can do a great job at communicating this effectively, it does not serve as an easy communication channel to others who are not well-versed in the programming language being communicated. The concept behind Coqui is that programs should be documents that describe what they do in layman's terms, and that the execution logic for said programs should be embedded into that document. In this sense, this brings comments and documentation to the forefront of what the program is. The goal of Coqui is to let programmers first describe their program in plain english, and then add syntactic structures to their english desription to make it executable.

## An Example

To understand the concept of Coqui Let's start by taking a look at a simple example program. For our example, we will describe how to compute "the nth fibonacci number". Let's start by just writing a plain english description of how this computation is performed.

```
We can find the nth fibonacci number as follows:

If n is less than 2, then finding the nth fibonacci number is arbitrary, as the result is just n itself.
Otherwise, we take the (n-2)th fibonacci number and the (n-1)th fibonacci number, and add them together.
```

We can now turn this into a Coqui program with very few changes to our description. That program is as follows:

```
define [%(n)th fibonacci number] as {
  If ?[$(n) is less than 2], then
    {finding the $(n)th fibonacci number is arbitrary, as %(the result :: is just $n itself)}.
  Otherwise, {
    we take @[the &[$n - 2]th fibonacci number] and @[the &[$n - 1]th fibonacci number].
    %(The result :: is [the sum of @them]).
  }
}
```

# TODO: extension
(see [./examples/fibonacci.coqui](./examples/fibonacci.coqui) for an executable version)

Note that there are no keywords in the definition of this computation. In other words, "if" and "otherwise" have no special meaning, it is merely the syntax wrapping the english description that provides the program its semantics. In theory, we could shorten the entire program by removing the english comments and it would still execute, but doing so would defeat the purpose of using Coqui.

While this was not implemented within the time constraints of the language jam, these programs can be easily converted into both static (latex/pdf) and interactive (html) documents improved readability. In the case of interactive documents, it is foreseeable that it would be easy to build a debugger that allows the programmer to navigate through the english language document and debug any issue in the interpretation of the program they are constructing. In theory, since this document contains now "actual code" and is just a rich text with links and metadata document, even a non-programmer could interact with such a debugger to find issues in the program and report them to a programmer to fix.

## Syntax and Semantics

Programs are composed of definitions of "phrases" which are given explicit computational meaning. The body of a definition is just plain english that describes how a given "phrase" can be computed. Within this plain english description of the computation, the programmer embeds syntactic structures that direct the computer how to parse and compute values from the english definition.

Below is a table of the various syntactic structures and their semantic interpretation:

| Syntax | Description |
|--------|-------------|
| `%(IDENT :: PHRASE)` | Introduce a new identifier to the program, which will be bound to the result of evaluating the specified phrase. |
| `$(IDENT)` | Refer to a previously bound identifier (can be used to access the value, but also can be used to just link back to the specific concept you are talking about in the coments). |
| `[...]` | Lookup and call another phrase in the program. |
| `^{...}` | Construct a value from a section of the document. |
| `?[...] ... {...} ... {...}` | Ternary. Lookup and call another phrase in the program and enter one of the specified branches based on the resulting value (boolean). |
| `{...}` | Escape some section of the document so that it is not interpreted (mostly used to escape references) |
| `%(IDENT)` | Only valid in patterns and definition headers. Binds the specified identifier in scope when the phrase is called or pattern is executed. |

Ordinal numbers (1st, 3rd, nth) and articles (the, a, an) have special rules so that programs can be interpreted without ambiguation but documents can still be written in descriptive natural language.

## Building and Running

1. Install [opam](https://opam.ocaml.org/doc/Install.html).
2. Create an opam switch by running `opam switch create langjam21 ocaml-base-compiler.4.12.0`.
3. Run `eval $(opam config env)` to finish setting up the switch in your shell (possibly not necessary).
4. Run `opam switch import opam.export` to install the necessary opam packages to build the project. 
5. Run `dune build langjam21.exe` to build the program.

Once you have built the program (which will appear in `./_build/default/coqui.exe`), you can execute programs through the command line using the syntax `coqui.exe <FILE> <PHRASE>`, where the `<PHRASE>` is the root phrase you want to evaluate. For example, try out the following commands:

```
$ ./_build/default/coqui.exe examples/fibonacci.coqui "the 10th fibonacci number"
$ /_build/default/coqui.exe -- examples/sum_of_squares.coqui "sum of the first 20 squares"
```

## Theoritcal Roadmap

- compile interactive documents
  - HTML pages that let you interact with it and hover-inspect values while executing/debugging
- imported dictionary support
  - eliminate need to explicitly provide verb tenses in majority of cases
- static semantic checks
  - check result semantics of blocks based on utilization
  - verify non-interpreted references are properly formed
  - well-directedness of definite vs. indefinite articles
  - etc...