# KerLang

KerLang is an innovative programming language built with love <3 for the [Langjam0001](https://github.com/langjam/jam0001). It features a state of the art compiler named "The Glorious KerLang Compiler" (also known as *the Glorious Ker-Lann Compiler* but who cares ?) and a first-class support for comment-oriented programming.

## What on Earth is *comment-oriented programming* ?

Writing good comments is CS 101. But we all know how hard it is to stay focus on writing comments and documentation. Comment-oriented programming propose a game changing approach to software development : instead of writing the code and then writing comments, KerLang enforce you to over-comment your code and the compiler generate the code for you !

Here is an example program :

```c
/*
	This function takes 1 argument,
	it returns the product of argument 1 and 2.
*/
function twice;
```

When executing a program written in KerLang, you first ask the compiler to generate code :

```ocaml
gklc file.kl
```

outputs :

```ocaml
let twice x = 2 * x
```

## Build instructions

KerLang is crafted in pure OCaml (version **4.11.1**) with no dependencies other than [dune](https://dune.build) as its build system.

### Installing OCaml and dune

To install a suitable version of OCaml from scratch, the following commands should do the job on any OS

```
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init
eval $(opam env)
opam switch create 4.11.1
opam install dune
```

### Compiling the project

Compiling the Glorious KerLang Compiler is as simple as typing `dune build` at the project's root !
