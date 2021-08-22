
# Build

Build using Haskell:

## Run alex and happy (Optional)
These are like lex and yacc, install them using `cabal install alex happy` and then call `alex Lexer.x` `happy Parser.y`.

This is optional as I have included the generated Haskell files

## Run GHC

`ghc REPL`

# Use

This is a pure functional language, see `fact.ct` for a simple example of a source file with declarations, then you can evaluate expressions in the REPL.

There are 3 modes of the REPL as demonstrated thus:

## Release
Here comments are ignored

```
% ./REPL release fact.ct
> fact 5
120
> fact_crash 5
Error: Invalid arguments to mul: 1, False
at:

> 
```

## Error Traceback
Here comments for functions are given in the traceback when an error occurs:

```
% ./REPL errorTraceback fact.ct
> fact 5
120
> fact_crash 5
Error: Invalid arguments to mul: 1, False
at:
 The factorial function on 1
 The factorial function on 2
 The factorial function on 3
 The factorial function on 4
 The factorial function on 5

> 
```

## Tracing
Here the comments for functions are printed at the start and end of the execution of the function

```
% ./REPL tracing fact.ct       
> fact 5
Begin: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| Begin: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | Begin: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | | Begin: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | | | Begin: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | | | | Begin: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | | | | End: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | | | End: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | | End: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| | End: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
| End: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
End: LiftToComment (App (Variable (Ident "makeFactComment")) (Variable (Ident "n")))
120
>
```

