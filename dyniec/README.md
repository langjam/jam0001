# nocomment
Idea of this language was to annotate expressions with expressions (thus no comments, just expressions).
For example
```
addition = add @ "This function is used to calculate sum of two numbers" @ ((addition 2) 5)
```
Annotations are right now dropped at runtime, but idea was to provide functions in runtime to instrospect annotations
## Installation steps
Install `cabal` and `ghc` using [ghcup](https://www.haskell.org/ghcup/) or system provided package manager.

Then execute `cabal update`, and then you can execute example programs using `cabal run dyniec examples/example1.l`
