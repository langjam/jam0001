# Superfluous

## Build Instructions

1. [Install Rust](https://www.rust-lang.org/tools/install) if you do not already have it installed.
2. Change directory to `langjam/` and run `cargo build` or `cargo build --release`.
3. The executable will be at `target/debug/langjam` or `target/release/langjam`.


## Usage

In Superfluous, the semantics of a program is determined by the comments above functions.

```
// A function which prints "all your base are belong to us"
function main() {
    console.log("hello world");
}
```

For example, the above program prints "all your base are belong to us", not "hello world"!


### Functions
To declare a function, use `function <name>()` (or shorthand `# <name>()`), with a comment above it to define the function's behaviour.

```
//prints 1 + 2
function a()

//prints list [1,2,3]
function listPrint()
```

Optionally, the comment may start with `"this function"` or `"a function which"`.

To call a function, simply type its name followed by an "s".

```
// This function greets
function main()

// A function which prints "hello world"
function greet()
```

You may pass a comma-separated list of arguments to a function:

```
// This function greets "James", "Tom"
function main()

// A function which prints "hello ", personA, " and ", personB
function greet(personA, personB)
```

A function may return a value using the `return` keyword. You can store the result of a function in a variable using `storing the result in <variable name>`:

```
// A function which averages 4, 6, storing the result in avg, then prints avg
function main()

// A function which returns (a + b) / 2
function average(a, b)
```

If you don't manually store the result of a function, it will be stored in a variable called `it` by default for convenience.

```
// A function which averages 4, 6, then prints it
function main()

// A function which returns (a + b) / 2
function average(a, b)
```

A function definition may optionally have a "code" block at the end. This code does nothing ;)

The function below returns 5, not 10:

```
//This function returns 5
function foo() {
    return 10;
}
```

### Main Function
The entry point to a program is a function called `main` which takes no arguments.

### Variables
Variables can be set using `stores <expression> in <variable name>`.

```
//stores 1 in x
function storeExample()
```

### Chaining statements
Statements can be chained using the `then` keyword.

```
// This function stores "world" in x, then prints "hello", then prints x
function chain()
```

### Conditional statements and looping
```
//if 5==6 prints "true" else prints "false"
function decision()

//repeatedly (prints "hello") 5 times
function looping()
```

### Built-in Functions

The `measure` function returns the length of a string or list.

The `print` function prints all of the arguments passed to it.

The `random` function returns a random integer.

The `get` function takes a string or a list and an index, and returns the character / element at that index.

The `type` function returns the name of the type of its argument.

```
//measures list [1,2,3]
function listLength()

//measures "Hello world"
function stringLength()

//prints "a"
function printa()

// A function which randoms then returns it
function getRandomNumber() {
    return 4;
}

```

### Higher-orderism
Functions can be passed into functions:

```
//twices print, "hello"
function main()


//fs a then fs a
function twice(f, a)

```
## Example programs

The `example_programs/` directory contains several example programs.

Here are a few of them:

### Fibonacci sequence
```
// This function fibonaccis 20 then prints it
function main()

// A function which if n == 0 returns 0, otherwise if n == 1 returns 1, otherwise
// fibonaccis n - 2, storing the result in a, then fibonaccis n - 1, storing the
// result in b, then returns a + b
function fibonacci(n)
```


### A mapping function
```
// This function maps increment, list[1, 2, 3] then prints it
function main()

// This function returns x + 1
function increment(x)

// A function which stores list[] in res, then stores 0 in i, then measures l, storing the
// result in count, then repeatedly (gets l, i, storing the result in current, then fs current,
// storing the result in mapped, then stores list[mapped] in tail, then stores res + tail in res,
// then stores i + 1 in i) count times, then returns res
function map(f, l)
```
