# Rundown

Rundown is a domain specific language for building Runnable Markdown.

The intent is to make a Literate programming language, where the whole program and text are interwoven in such as way that the code is legible.

This README is also a Rundown script, try running it!

More examples can be found in the `examples` directory.

To execute a Rundown Script, run `cargo run /path/to/script` from the project directory.

## Rundown Question

Would you like to hear more about Rundown? [Yes/No]

```rundown
let response = read();
if (response == "no") {
    goto "end";
}

if (response == "yes" ) {
    goto "first-class-comments";
}

goto "rundown-question";
```

## First class comments

In our view, comments are anything in a source file meant not for execution, but rather human readability and understanding.
In that case, nearly the whole source file is just meant for human reading.
The code is minimal, and the focus is on readability.
In fact, the source only serves to modify what is read.

### First class Question

Do you want to see some examples or have you seen enough? [Examples/End]

```rundown
let response = read();
if ( response == "examples" ) {
    goto "examples";
}

if (response == "end") {
    goto "end";
}

goto "first-class-question";

```

## Examples

Being constrained to markdown is no problem.
Rundown can jump to anywhere in the document.
Let's look at a couple of examples.

### Examples Question

Which example would you like to see? [FizzBuzz/Hello World/Continue/End]

```rundown
let response = read();
if (response == "fizzbuzz") {
    goto "fizzbuzz-example";
}

if (response == "hello world") {
    goto "hello-world";
}

if (response == "continue") {
   goto "features";
}

if (response == "end") {
    goto "end";
}

goto "examples-question";
```

### FizzBuzz Example

FizzBuzz can be trivially done using strings all in code.
However, the point of this language is to jump to points in a markdown file, and display it.
In this example, we have 3 different headings: Fizz, Buzz, and FizzBuzz.
The code will execute in a goto loop, jumping into the categories and printing the labels.

The static variable is only set once in a code block, even if that code block is executed multiple times.
This example works by jumping to a section, printing the text of that section, then jumping back.

#### FizzBuzz Code

```rundown
let global counter = 0;
counter = counter + 1;
if (counter == 100) {
    sleep(1);
    counter = 0;
    goto "examples-question";
}

if (((counter % 3) == 0) && (counter % 5) == 0) {
    goto "fizzbuzz";
}

if ((counter % 3) == 0) {
    goto "fizz";
}

if ((counter % 5)  == 0) {
    goto "buzz";
}

print(counter);
goto "fizzbuzz-code";
```

#### Fizz

Fizz

```rundown
goto "fizzbuzz-code";
```

#### Buzz

Buzz

```rundown
goto "fizzbuzz-code";
```

#### FizzBuzz

FizzBuzz

```rundown
goto "fizzbuzz-code";
```

### Hello World

There are two ways to do Hello World.
The First is to recognize Rundown programs are Markdown, and simply write `hello world` in a markdown document.
That is a bit simplistic, let's print it from a rundown code block.

```rundown
print("hello world");
```

We can even print valid markdown!

> Note: You cannot print headings, then goto them

```rundown
print("**Hello World**");
```

Let's head back to the examples.

```rundown
sleep(1);
goto "examples-question";
```

## Features

The Rundown language is fairly simple.

Let's walk through how to use it.

### Writing rundown

Rundown is written in Markdown code blocks.
Just add the `rundown` to the code block info string.

```rundown
let a = 1;
print(a);
```

### Variables

Valid variable names start with a letter, but are alphanumeric afterwards.
For example, `a1` is a valid variable. `1a` is not.

How you declared a variable impacts it's scope.

1. `let a = 1;` will declare a variable local to a code block.
1. `let static a = 1` will declare a variable that persists in a code block through uses.
   It will only be declared once for a code block, but the value persists.
1. `let global a = 1` declares a variable that can be used in all code blocks.

Variable values can be modified through assigning.
Simply use the variable name `=` and an expression.
`a = 2`, or `a = some_func()`, etc.

### Branching

Branching is done with if statements, and goto.

#### If

There are a few normal logical operators.
However, When joining multiple expressions, if the left hand side is itself multiple expressions, it should be wrapped in parenthesis.

- `||`
- `&&`
- `!`
- `==`
- `!=`

```
let a = 1;
if (a == 1) {
    print("A is 1");
} else {
    print("A is not 1");
}
```

```
let a = 1;
let b = 2;
if ((b - 2) == 0) && a == 1) {
    print("B - 2 is 0 and A is equal to 1");
}
```

#### Goto

Goto statements jump to headings in Markdown.

For example, `goto "goto"` would jump to this section in markdown.
There are no labels in the code, only in markdown.

### Loops

Looping can be done with goto statements.

For example, this code would create an infinite loop over this section.

```
goto "loops"
```

### Operators

There are a number of arithmatic operators.

- `+`: add
- `-`: subtract
- `*`: multiply
- `/`: divide
- `%`: modulo

Additionally, you can use `-` as a negative operator on a single value.

#### Functions

Functions can be defined with the following syntax.

```
fun name(param1, param2, param3) {
    x = param1;
    y = param2;
    z = param3;
}
```

Calling functions is just an expression.

```
name(arg1, (2 - 2), arg3);
```

##### Built-Ins

There are a few builtin functions for ease of use.

- `read()` will read input from stdin until a newline.
- `sleep(n)` will pause execution for n seconds
- `print(expr)` will print the result of some expression

## End

Thanks for checking out **Rundown**!
