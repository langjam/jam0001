# FCC - a language in which comments can be attached to any first class citizen , so it makes them first class too.

## Compile

To compile you need ghc and cabal installed , it is a haskell project.

  cd fcc
  cabal update
  cabal build

## Run

To run the interpreter you need to pass a source (.fcc) file on the cmdline.
Since it is a cabal project , use cabal to execute.

  cabal exec fcc -- moop.fcc

## FCC Language

The language is a strongly typed lamda calculus extension, adding some suger , and of course the
ability to "attach" comments to any first class value. There is no repl yet, so you have to have
a "main" entrypoint in your sourcefile.

### Source Code Example

  module [Add|Time to add numbers] where
  

  [add a b| adding two numbers in a way that shows some language features] = 
      [\c d -> let [u| first number] =  c
                   [v x|function to return a number as is] = x
               in u + (v d)
      |lamda adding two numbers] a b

  [main|Lets go] = add 1 2




