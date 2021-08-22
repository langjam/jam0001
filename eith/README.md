# Eith

A Forth-like language where the comments matter.

Made for [LangJam #0001](https://github.com/langjam/jam0001).

## What is this?

Eith is a Forth-like language that allows for comment reading and manipulation.
In particular, language implements a subset of Forth's Core words as well as the Eith Comment words.

Eith provides all the familiarity of a traditional stack-based language and combines it with the
power of dynamically modifiable source code.

## Getting Started

Several sample programs can be found under [`samples`](samples/).

Eith programs can be run with the provided interpreter:

```
python eith.py <PATH TO EITH SOURCE FILE>
```

## Limitations

Eith does not implement all of the Forth Core. It is notably missing any words that
deal with memory and addressing, several multi-word constructs, and user-defined words.

Due to interpreting line-by-line, Eith also requires that each line must be independently interpretable.
This means that certain constructs, such as `IF ... THEN` or `DO ... LOOP` must be on one line.