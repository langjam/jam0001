# 〈( \^.\^)ノ

〈( \^.\^)ノ the best think that happened since [blank](https://www.youtube.com/watch?v=dQw4w9WgXcQ) and an interpreted, **not** esoteric langanguage for writing complex **safety** oriented applications in large production environments.
It was created for [langjam0001](https://github.com/langjam/jam0001) by team ``〈( ^.^)ノ``.
It is based around two tapes for accessing variables, arguments, etc. and an interesting comment-first function approach, that allows for the dynamic creation of functions.

## How to set up 〈( \^.\^)ノ
〈( \^.\^)ノ is written in go and will thus require the standard golang tools to be installed.
Documentation on how to install go can be found [here](https://golang.org/doc/install)

As soon as everything is set up you can check if it works by running:
```bash
go run . --version
```
Next up, let's try to get a basic hello world example to run.
Here our syntax should be fairly straigth forward (and can also be found in our ``examples`` directory, right [here](/%E3%80%88(%20%5E.%5E)%E3%83%8E/examples/helloworld.sml)):
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

We'll showcase most of the features using the following fibonacci sequence program (can also be found [here](/%E3%80%88(%20%5E.%5E)%E3%83%8E/examples/fib.sml)):
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
As can be seen above 〈( \^.\^)ノ has a number of directives:
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
Variables in 〈( \^.\^)ノ are are based on two sets of registers, one in the negative namespace and one in the positive namespace.

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
Goal of the langjam was to create a language with ``first-class comments``, our implementation of comment-functions was the attempt to accomplish exactly this.
With the comments in 〈( \^.\^)ノ, you can annotate and chain together statements.
Using the identifiers in the comments you can then actually reference, assign and pass around the statements annotated by a comment.

Statements can be annotated by comments by just writing a comment after a statement:
```go
set($-3 $-2) // save
```

If you wish to annotate multiple statements, you have to make this clear by using the ``and`` instcuction:
```go
set($-3 $-2) // save
and(print($-3)) // save
```
The interesting part here is that this can be done dynamically and at runtime!

Furtherly, like any first-class citizen, these functions can be stored in variables:

```go
set($-1 "save")
$-1()
```

As can be seen in the fib example, you can also chain together comments using ``,`` or ``and`` while calling them:
```go
set($-3 $-2) // save
set($-2 m("$-2 + $-1")) // load
set($-1 $-3) // calc

// MAGIC happens right here:
"save, calc, load"() // fib
```

### Loops
〈( \^.\^)ノ only needs while loops to fuction.
Simmilarly to many other languages, while loops are declared by writing the while keyword followed, by an expression in parenthesis and a block of statements in curly brackets:

```go
while (TRUE) {
  "fib"()
  print($-1)
}
```

#### Break
Sometimes however, you may want a loop to end.
For these cases, you can use the ``break()`` instuction.

Unlike other instructions, this one *allways* executes at the end of the loop, regardless of where you write it
```go
while (TRUE) {
    print("hello")
    break()
}

// is the same as:

while (TRUE) {
    break()
    print("hello")
}
```
#### Conditional loops
Using a these break instructions, you can implement if-statement-like conditional loops:
```go
while (m("1 > 2")) {
    print("2 is bigger than 1")
    break()
}
```

### Constants
As can be seen in the many examples above, we also happen to have two constants ``TRUE`` and ``FALSE``. 
Not much to explain here, but you can use them in while statements:
```go
while (TRUE) {
// To infinity and beyond
}
```

### Arrays
