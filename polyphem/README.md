# FCC - a language in which comments can be attached to any first class citizen , so it makes them first class too.

## Compile

To compile you need ghc and cabal installed , it is a haskell project.

```
cd fcc
cabal update
cabal build
```

## Run

To run the interpreter you need to pass a source (.fcc) file on the cmdline.
Since it is a cabal project , use cabal to execute.

```
cabal exec fcc -- moop.fcc
```

## FCC Language

The language is a strongly typed lamda calculus extension, adding some suger , and of course the
ability to "attach" comments to any first class value. There is no repl yet, so you have to have
a "main" entrypoint in your sourcefile.

### Source Code Example
```
module [Moop| mooping now for you] where


[i| An integer constant] = 1+3

[j x| J Function adding 1 with a lambda to arg] = [\a -> 1+a|the inc lambda] x

[main|Here we go] = j i


```
If run this will yield :

```
➜  fcc git:(master) ✗ cabal run  fcc -- moop.fcc
Up to date
"Moop: mooping now for you"
Cmt "Here we go"
Cmt "J Function adding 1 with a lambda to arg"
Cmt "An integer constant"
Cmt "the inc lambda"
VNum 5
➜  fcc git:(master) ✗ 
```




