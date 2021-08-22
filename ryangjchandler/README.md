# The Decor Language

The name originates from the term "decorators". Some languages use decorators to provide extra meta-data about functions, variables, etc. In a sense, they're a type of comment.

I took this a step further by making them a core part of the language. It completely changes the way you write your functions and declare variables / constants.

## Build Steps

This project is built with Rust. It only has 2 direct dependencies, so build times are quick.

For ease-of-use, I'd recommend running the project with `cargo`.

## A minimal example

If you run `cargo run --release` in the root directory, you'll likely receive an error message about a filepath being needed.

For testing purposes, a minimal example file can be found inside of `examples/minimal.decor`. Run this file:

```
cargo run --release -- ./examples/minimal.decor
```

This won't output anything in the console / terminal, but if you add the `--info` flag to the end of the command, you'll receive some basic output.

```
cargo run --release -- ./examples/minimal.decor --info
```

This is being pulled from the "file header". This block-like comment acts as the descriptor for the document.

### Directives

Directives are identifier prefixed with an `@` symbol. These are special keywords recognised by the language and can be used to manipulate behaviour and expectations.

Here is a list of valid file headers:

* `@name` - The name of the script.
* `@description` - A description of the script.
* `@author` - The author of the script.
* `@version` - The version of the script.

All of these file directives should be followed by an enclosed string, e.g.

```
/*
 * @name "My Example Script"
 * @description "This is an example script."
 * @author "Ryan Chandler <support@ryangjchandler.co.uk>"
 * @version "1.0.0"
 */
```

If you execute this code, all of the information will be displayed with some pretty colours.

## Types

Decor only supports a handful of types:

* `string` - A normal string type, wrapped with `"` characters. Supports escaped characters for line-breaks, etc.
* `number` - Similar to JavaScript, both integers and floats are stored as the same type internally (64-bit floating point).
* `bool` - The standard `true` and `false`.
* `null` - Not a huge fan of it, but it's here.
* `list` - An array structure to hold an arbitrary number of items with an arbitrary number of types.

Here is an example of each type:

```
"This is a string"
1234
12345.6789
true
false
null
["This is an array", 1234, false, null]
```

## Statements

Decor is a statement first language. Standalone expressions are stored inside of a `Statement`, making evaluation incredibly simple.

### Variable declarations

Variables declarations begin with the keyword `var`, followed by any expression. This expression will be used to initialize the variable.

A variable declaration **must be** preceded by a "declaration header". This is a special type of comment that can augment the behaviour of a particular statement.

```
/*
 * @identifier names
 */
var ["Ryan", "JT"]
```

In the example above, a new variable `names` will be created and it will hold a `list` of names `["Ryan", "JT"]`.

Here is a list of valid directives that can be used to change the way a `var` statement behaves:

* `identifier` - This should always be present as it determines the name of the variable.
* `type` - This can be used to specify which type the variable should hold. (Completely optional, types are only checked at initialisation due to time-limitations).

Here's an example:

```
/*
 * @identifier names
 * @type list
 */
var ["Ryan", "JT"]
```

If you tried to assign a `string` to this variable, it would trigger an error and prevent execution.

#### Assigning a new value

You can assign a new value to a `var` the same way as other languages:

```
/*
 * @identifier names
 * @type list
 */
var ["Ryan", "JT"]

names = ["John", "Jill"]
```

#### Constants

If you would like to declare a value as constant, you can replace `var` with `const`. This will prevent reassignments in your script.

### Function declarations

Function declarations begin with the keyword `fn`, followed by a block of statements. These statements act as the function body.

Just like a `var` statement, a function declaration **must be** preceded by a "declaration header". Here is a list of directives that can be used:

* `@identifier` - This is required and will be used as the name of the function.
* `@param` - Can be used multiple times to define multiple parameters. Can also be used to add types to parameters. (Optional)

Here's an example of a function that prints out `Hello, [name]`:

```
/*
 * @identifier say_hello_to
 * @param name string
 */
fn {
    println("Hello, " + name)
}
```

This function can be called like this:

```
say_hello_to("JT")
```

The `string` type of the parameter can be omitted. Types are only checked when being provided to the function. Any re-assignments to that property inside of the function will **not** trigger a type-check.

### If/else

The if/else statement is fairly standard. Here's an example:

```
if true {
    println("It's true!")
} else {
    println("It's false...")
}
```

The condition does not need to be wrapped in `()`.

Decor **does not** support `else if` arms. It's a feature, not a bug. ;)

### While

`while` statements can be used to loop conditionally.

```
while true {
    println("We loved infinite loops!")
}
```

Due to my own personal time limitations, `continue` and `break` statements are not supported.

### For in

The `for..in` statement can be used to loop over a list of items, as well as a string.

```
for name in names {
    println(name)
}

for i, name in names {
    println(i + " " + name)
}
```

> When iterating over a `string`, the string's characters will be used.

## Supported operators

* `+`, `-`, `*`, `/`, `&`, `|` - mathematical operators.
* `&&`, `||` - boolean operators.
* `+` can be used to concatenate 2 strings or 1 string and 1 number.

## What I wanted to do but couldn't find the time for

1. `return` statements inside of functions.
2. `break` and `continue` statements inside of loops.
3. `map` type for key-value pairs.
4. Add more standard-library functions, only one that comes out of the box is `println` for console output.
5. Add more mathematical operators
6. Provide a mechanism for executing a function from the command-line directly, i.e. `decor ./my_script.decor my_function_name`. This would have been great for reusable scripts with multiple entrypoints.