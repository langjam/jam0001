# Comstruct

### The Constructor is named `comment` / Probably some other things too

## Content

1. [Compiling the Interpreter](#Compiling-the-Interpreter)
2. [Starting the Interpreter](#Starting-the-Interpreter)
3. [Variable Assignment](#Variable-assignment-)
4. [Variable calling](#Variable-calling-)
5. [Arithmetic operations](#Arithmetic-Operations-)
6. [Function Definition](#Function-Definition-)
7. [Function Calling](#Function-Calling-)
8. [For Loops](#For-Loops-)
9. [Forever Loops](#Forever-Loops-)
10. [If Statements](#If-Statements-)
11. [Internal Functions](#Internal-Functions-)
12. [Booleans](#Booleans)
13. [Classes](#classes)
14. [Comments and Function Signatures](#Comments-and-Function-Signatures)
15. [Examples](#Examples)

## Compiling the Interpreter

![](https://img.shields.io/badge/Tested-Arch%20Linux-green)

You can compile the interpreter into a single executable, but you don't have to. You can either execute
the `comstruct.py` file from the `src`-directory or the built binary in `dist/`.

On (pretty much) any sytem with python 3.9 and all the [requirements](requirements.txt) installed, in (pretty much) any
shell go to the Floeckchengrafik/src folder of the cloned repo and
run `pyinstaller comstruct.py application_stack_utils.py executor.py internals.py lexer.py parser.py -c -F -p . --distpath ../dist --collect-all sly`
.

We have no idea if it'll run on other platforms too. We (Chris and Flo) both [use arch btw](https://i.redd.it/z22mv8adan771.png). If you have any suggestions
or tests for other platforms, we'll be happy to add them to this readme. Don't hesitate to open an issue
on [this repo](https://github.com/Floeckchengrafik/jam0001).

## Starting the Interpreter

If you [compiled the Interpreter](#Compiling-the-Interpreter), you can run the binary file in `dist/` like this:
`./comstruct example.cmstr`<br>
If not, you can run `python src/comstruct.py example.cmstr`.<br>
Of course, replace example.cmstr with whatever your filename is. When you don´t have a .cmstr file to execute, you can
also run `./comstruct` in `dist/` or `python comstruct.py` in `src` to get to the repl.

# Code Documentation

## Basics

### Variable assignment: <br>

|Format|Example|
|---|---|
|`<name> = <assignment>?`|`foo = 1?`|

### Variable calling: <br>
|Format|Example|
|---|---|
|`<name>?`|`foo?`|
<br>
Note: variables can also be used in arithmetic operations, e.g. 1 + x - y?


### Arithmetic Operations: <br>
|Format|Example|
|---|---|
|`<var1> <operation> <var2>?`|`1 + 1?`|
<br>
Note: operation can be one of the following characters: +, -, *, /, %

### Function Definition: <br>
|Format|Example|
|---|---|
|`<name> = function({ /* function body */ })?`|`foo = function({out("I am here!!1!")?})?`|

### Function Calling: <br>
|Format|Example|
|---|---|
|`<name>([args])?`|`foo()?`|

### For Loops: <br>
|Format|Example|
|---|---|
|`for("<name>" : <operation> : { /* loop body */ })?`|`for ("x" : intrange(0:20) : { out("This is test nr. " : y)? })?`|

### Forever Loops: <br>
|Format|Example|
|---|---|
|`forever({ /* loop body */ })?`|`forever ({ out("This Loop will run infinite")? })?`|

### If Statements: <br>

|Format|Example|
|---|---|
|`if(<condition> : { /* statement body */ }?`|`if (1 == 1 : {out("1 is 1!")? })?`|

In If Statements you can use following operators for your operation: `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!!`

### Internal Functions:

#### out()

- prints something to the console
- args:
  1. The message that should be printed in the console

#### readline()

- reads userinput from the console
- args:
  1. The Message that should stand in front of the user´s cursor
#### length()

- gets the length of a list
- args: 
  1. the list of which the length should be returned
#### list_add()

- adds an element to a list
- args:
  1. The object that should be appended to the list
  2. The list the object should be appended to
#### list_clear()

- clears the list
- args:
  1. The list that should be cleared
#### list_extend()
  
- Extends the list with the given item
- args:
  1. The list that should be extended
  2. The element that the list should be extended with
#### list_pop()
  
- Takes out the given element of the list, when nothing is given, it pops the first element
- args:
  1. The list from which the element should be popped
  2. [Optional] The element that should be popped
#### list_reverse()
  
- Reverses the given list
- args:
  1. The list that should be reversed
#### list_sort()
  
- Sorts the list
- args:
  1. The list that should be sorted
#### list_get()

- Gets an element from a list
- args:
  1. The list from which the element should be taken
  2. The element index that should be taken
#### list_set()
  
- Sets an element of a list
- args:
  1. The list in which the element should be inserted / replaced
  2. The element index that should be inserted / replaced
  3. The insertment / replacement
#### intrange()

- returns a list of integers
- args:
  1. The minimum (inclusive)
  2. The maximum (exclusive)

#### intrange_inclusive()

- returns a list of integers
- args:
  1. The minmum (inclusive)
  2. The maximum (inclusive)

#### string_startswith()

- Checks if the string starts with a given prefix
- args:
  1. The string to check
  2. The prefix

#### string_endswith()

- Checks if the string ends in a given suffix
- args:
  1. The string to check
  2. The suffix

#### string_islower()

- Checks if the string is lowercase
- args:
  1. The string to check

#### string_isupper()

- Checks if the string is uppercase
- args:
  1. The string to check

#### string_lower()

- Sets a string to lowercase
- args:
  1. The string to change

#### string_upper()

- Sets a string to uppercase
- args:
  1. The string to change

#### string_removeprefix()

- Removes a given prefix of a string
- args:
  1. The string to change
  2. the prefix to remove

#### string_removesuffix()

- Removes a given suffix of a string
- args:
  1. The string to change
  2. The suffix to remove

#### string_replace()

- Replaces something in a string with something else
- args:
  1. The String to change
  2. The old string
  3. The replacement string

#### string_split()

- Splits a string by the given characters
- args:
 1. The string to split
 2. The separator

#### string_contains()

- Splits a string by the given characters
- args:
 1. The string to check
 2. The part string

### None
None is the keyword for a non-existing Value, equal to null in other languages

### Booleans

#### true
#### false

A boolean is a value that could be either true or false

### Classes

|Format|Example|
|---|---|
|`<name> = class({ /* class body */})?`|`foo = class({ comment = function({ out("Hello World")? })? })?`|

To Instance a class, use `<name>([args])?`<br>
To run a class method, use `<class_instance_name>.<method_name>([args])?`

#### Constructors
You define a constructor using `comment = function ({ /* Constructor body here */ })?`.
You name the constructor comment, because the theme of [this jam](https://github.com/langjam/jam0001) is "first-class-comments", and the constructor is the `first` thing that gets executed in a `class`.

## Comments and Function Signatures

For a single-line-comment, write `\\`
To start a multi-line-comment, write `/*`. To end it, write `*/`

Function Signatures are needed when you want to pass a variable to a class or method. You can start a Function Signature
with `/*` and end it with `*/`.<br><br>To pass an argument to a class / function, you write `- param <name>` in a new
line. To allow a return value from the function, you write `- return` in a new line. The last statement in the function
will be evaluated and returned.

## Examples

In this repo, there are 9 examples:

- [Simple Maths and Operations](samples/1_simplemath.cmstr)
- [A Hello World, implemented with functions and String Concat](samples/2_helloworld.cmstr)
- [If and For-Loops (and exit)](samples/3_if_and_for.cmstr)
- [Playing around with lists](samples/4_lists.cmstr)
- [Functions and how they are defined](samples/5_functionsignatures.cmstr)
- [Classes](samples/6_classes.cmstr)
- [Additional String Methods](samples/7_string.cmstr)
- [The Forever-Loop](samples/8_forever.cmstr)
- [A fully in the command line playable version of Sokoban](samples/9_sokoban.cmstr)

If you have any ideas for other examples, just create an issue or a pullrequest.
