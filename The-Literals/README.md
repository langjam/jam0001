# Commentinator

Welcome to **Commentinator**, a language for the first [langjam](https://github.com/langjam/jam0001), a 48-hour language jam in which the theme was "first-class comments".

### Authors
Written by [badlydrawnrod](https://github.com/badlydrawnrod|badlydrawnrod) and [Lorne Hyde](https://github.com/LorneHyde).

## Inspiration
Bad comments are worse than no comments at all. A particularly egregious style of bad comments is the one in which comments merely describe what the next line is doing, without providing any context.

For example:
```cpp
// Increment the counter.
++counter;
```
Commentinator is inspired by this commenting style, and makes such comments executable. It may be possible one day to write a program that is valid in both Commentinator and another language.

# Running Commentinator
## Pre-requisites
Commentinator requires Python 3.6 or better to run, mostly because it uses f-strings. It does not require any external packages.

## Platforms
The authors have successfully run Commentinator under Python 3.8.5 on **Ubuntu** and under Python 3.9.6 on **Windows 10**.

## Usage
Simply run Commentinator as follows, making sure to substitute `python3` with the command line for running Python 3 on your system.

```
$ python3 commentinator.py [filename]
```

Alternatively...

```
$ python3 -m commentinator [filename]
```

# Examples
There are a number of samples in the `samples/` directory. At the time of writing, the following samples are known to work.

- `echo.comment` - demonstrates jump statements by echoing a string n times.
- `fib.comment` - calculates the nth fibonacci number and prints it using a **Built-in**.
- `multiply.comment` - multiplies two numbers and prints the result using a **Built-in**.
- `string_literal.comment` - prints a string literal using a **Built-in**.
- `user_input.comment` - asks the user for a number, doubles it, and prints the result.

### echo.comment
```
$ python3 -m commentinator samples/echo.comment
Hello
Hello
Hello
Hello
Hello
```
### fib.comment
```
$ python3 -m commentinator samples/fib.comment
55
```

### multiply.comment
```
$ python3 -m commentinator samples/multiply.comment
27
```
### string_literal.comment
```
$ python3 -m commentinator samples/string_literal.comment
Hello
```

### user_input.comment
```
$ python3 -m commentinator samples/user_input.comment
Please enter a number:55
110
```
# Syntax Guide

## Function Definitions

Commentinator supports multiple, recursive functions. These consist of a **Function Header** followed by a **Function Body**.

## The Function Header
Functions are introduced with a Javadoc-like function header. A full example is shown below.

```
/**
 * Multiplies two numbers.
 *
 * @param the first number
 * @param the second number
 * @return the result
 */
```

### Start of Function Header
The keyword that introduces a function is "`/**`". This is a slash followed by two or more asterisks.

### The Function Name
The function name is the text on the first line of the function header. This must be a third person singular construct, starting with the verb and ending with a full stop (period). In the example above, the function name is `Multiplies two numbers`.

### Parameters
Function parameters are specified by means of the `@param` keyword. These must be on separate lines, following the function name. Parameter names may contain spaces, but no punctuation is permitted. In the example above, the parameter names are `the first number` and `the second number`.

Do not supply the `@param` keyword if the function has no parameters.

### The Return Value
If the function returns a value, then this is specified via the `@return` keyword. This takes a mandatory name. Return value names may contain spaces, but no punctation is permitted. In the example above, the return value's name is `the result`.

### End of Function Header
Function headers are terminated by "`*/`". This is a single asterisk followed by a slash on a line by itself.

## The Function Body

A function body consists of zero or more **Statements**, terminated by a mandatory **Terminator Statement**.

## The Terminator Statement
The function terminator statement is a single line comment, followed by the phrase `And we're done.`

```
// And we're done.
```

## Statements
Statements are always introduced by a single line comment, "`//`" and are terminated by a full stop (period). All statements may be concluded by an optional **Inline Terminator Statement** which is the phrase `and we're done`. This acts a return statement in a function, and will terminate the program if called from the **Main Block**.

## If Statement
An if statement consists of an `If` keyword, followed by a **Condition**, a `then` keyword, then a statement which is executed if the condition evaluates to true.

```
// If n is 0 then set fib to 0 and we're done.
```

## Variable Assignment Statement
A variable assignment statement takes the form `Set`, followed by an identifier (which may contain spaces) followed by the keyword `to`, followed by an **Expression** or a string literal such as `"Hello"`.
```
// Set x to 5.
```

## Function Call Statements
A function call statement consists of a function name, in imperative form, followed by an optional argument list, followed by an optional **Postfix Assignment**. An example is shown below.

```
// Calculate the nth fibonacci number with n as 10 and call it fibonacci number.
```

## Jump Statements.
A jump statement consists of the keyword `jump`, followed by a direction (one of `forward` or `backward`), a number of lines to jump, then the keyword `lines`.
```
Jump forward 3 lines.
```

### Function Names
As described above, function names are given in third person singular form, e.g., `Opens the pod bay doors`. However, functions must be invoked in imperative form, e.g., `Open the pod bay doors`.

The following example invokes a function named `Calculates the nth fibonacci number`, and sets the named parameter `n` to 10. The result is placed into a variable called `fibonacci number`. Note that function invocation is imperative, even though the function name is third person.__*This is a revolution in imperative programming*__.

### Argument List
If present, this is a number of bindings from expressions to named function parameters. The first binding takes the form `with` *parameter name* `as` *expression*. Subsequent bindings are similar, but `with` is replaced by `and`.

For example, if a function has named parameters `x`, `y`, and `z`, then these could be supplied in order as follows:
```
with x as 3 and y as 22 and z as 99.
```
Or in a different order:
```
with y as 22 and z as 99 and x as 3.
```

### Postfix Assignment
This is the equivalent of a variable assignment statement, but is used for the result of a function call. If present, it follows the function name and its optional parameters, and takes the form `and call it` followed by a variable name.
```
and call it the result.
```

## Built-ins
There are a number of built-in functions.

- `Prints a number`, which is invoked as `Print a number`.
- `Prints`, which prints a string literal and is invoked as `Print`.
- `Asks the user for a number`, which is invoked as `Ask the user for a number`.

## Expressions
Expressions in Commentator are exceedingly basic. They can be a variable or a number, or a binary operator where the operands are variables or numbers. There is no operator precedence, because an expression can only contain one binary operator.

## Comments
All lines that are not part of a Function Header, or are not introduced by a single line comment `//` are ignored.
