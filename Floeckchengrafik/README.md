# Comstruct

### The Constructor is named `comment` / Probably some other things too

## Content

1. [Compiling the Interpreter](#Compiling-the-Interpreter)
2. [Variable Assignment](#Variable-assignment-)
3. [Variable calling](#Variable-calling-)
4. [Arithmetip operations](#Arithmetic-Operations-)
5. [Function Definition](#Function-Definition-)
6. [Function Calling](#Function-Calling-)
7. [For Loops](#For-Loops-)
8. [If Statements](#If-Statements-)
9. [Internal Functions](#Internal-Functions-)

## Compiling the Interpreter

![](https://img.shields.io/badge/Tested-Arch%20Linux-green)

You can compile the interpreter into a single executable, but you don't have to. You can either execute
the `comstruct.py` file from the `src`-directory or the built binary in `dist/`.

On (pretty much) any sytem with python 3.9 and all the [requirements](requirements.txt) installed, in (pretty much) any
shell go to the Floeckchengrafik/src folder of the cloned repo and
run `pyinstaller comstruct.py application_stack_utils.py cli.py executor.py internals.py lexer.py parser.py -c -F -p . --distpath ../dist --collect-all sly`
.

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
|`<name> = function({ /* function body */ } [ : ARG_1 : ARG_2 : ARG_3 : ...] )?`|`foo = function({out("I am here!!1!")?})?`|

### Function Calling: <br>
|Format|Example|
|---|---|
|`<name>([args])?`|`foo()?`|

### For Loops: <br>
|Format|Example|
|---|---|
|`for("<name>" : <operation> : { /* loop body */ })?`|`for ("x" : intrange(0:20) : { out("This is test nr. " : y)? }?`|

### If Statements: <br> 
|Format|Example|
|---|---|
|`if(<condition> : { /* statement body */ }?`|`if (1 == 1 : {out("1 is 1!")? })?`|

### Internal Functions: <br>
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
#### list-pop
  
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

### intrange_inclusive()

- returns a list of integers
- args:
  1. The minmum (inclusive)
  2. The maximum (inclusive)
