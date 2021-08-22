# 〈( \^.\^)ノ

insert-name-here is the the best think that happened since [blank](https://www.youtube.com/watch?v=dQw4w9WgXcQ) and an interpreted, **not** esoteric langanguage for writing complex **safety** oriented applications in large environments.
It was created for [langjam0001](https://github.com/langjam/jam0001) by team ``〈( ^.^)ノ``.
It is based around two tapes of registers for accessing variables, arguments, etc. and an interesting comment-first function approach, that allows for the dynamic creation of functions.

## How to set up insert-name-here
insert-name-here is written in go and will thus require the standard golang tools to be installed.
Documentation on how to install go can be found [here](https://golang.org/doc/install)

As soon as everything is set up you can check if it works by running:
```bash
go run . --version
```
Next up, let's try to get a basic hello world example to run.
Here our syntax should be fairly straigth forward (and can also be found in our ``examples`` directory):
```go
print("Hello World!")
print(smile())
```
Run the programm by calling:
```bash
go run . path_to_the_file
```

## How to write code
Now that we've got a basic "Hello World" programm to work, we can start talking about the other language features and how to write more complex programms.

We'll showcase most of the features using the following fibonacci sequence program:
```go
set($-1 1)
set($-2 1)

set($-3 $-2) // save
set($-2 m("$-2 + $-1")) // load
set($-1 $-3) // calc

"save, calc, load"() // fib

while (TRUE) {
  "fib"()
  print($-1)
}
```

### Instructions
As can be seen above insert-name-here has a number of directives:
1. ``set``: used to set variables, parameters can be strings (``"hello World"``) or numbers
2. ``m``: takes in an arithmetic expression, such as ``8 + 9 * 1`` and evaluates it
3. ``not``: inverts booleans (or in our case ints) simmilarly to ``!`` in c
4. ``break``: breaks out of a while loop
5. ``smile``: returns a random emoticon
6. ``and``: allows chaining to gether of comment-functions (more on these later)
7. ``print``: prints the given input, also evaluates a string like ``$-1 is cool`` to whatever the value of $-1 is
8. ``place``: a set instruction for arrays
9. ``peek``: a get instruction for arrays
10. ``grow``: an instruction for for expanding arrays
11. ``len``: gets the length of a given array

As can be seen in the example above an instruction is allways used by writing out the identifier followed by the arguments to that instruction in parenthesis:

```go 
set($-1 420)
```

### Variables
Variables in insert-name-here are are based on two sets of registers, one in the negative namespace and one in the positive namespace.

Both can be acessed by ``$num``

Negative variables are globally accessible and act like variables in the "traditional" sense. 
Positive variables on the other hand represent the arguments of comment-functions, which will be explained in the next section.

Variables can be used in a multitude of ways:
```go
set($-1 1)

// returns 10
m("$-1 + 9")

// returns 1
print("$-1")
print($1)

// etc...
```

### Tapes

Tapes are the only way to store values (except not). You can address them using `$` and a number.
If the number is positive, you reference the positive tape. If the number is negative, you reference the negative tape.
If the number is 0, you reference the global variable.

Tapes are limited by your pc's ram and resized as needed.

#### Scoping

0 is global across the program.
The negative tape is shared for a single comment call.
The positive tape is used for arguments. When a comment is called,
arguments are put there. After a subcomment is executes, all arguments
are shifted so no used arguments stay.

Example:

```
m("$1 * $1") // square
m("$1 * 2") // double

print("square and double"(10 20))
```

Will print:

```
100
40
```


### Comments

### Loops

### Constants

### Arrays
