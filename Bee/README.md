# Bee Programming Language

Bee is interpreted programming language for LangJam 2021 (Created in less than 48h) based on first-class comments theme, the design of the syntax inspired from the comment style from many languages

## How to use it

In the bin directory you will find bee-lang.jar and example directory, the language require JAVA 8

to run any example

```
java -jar bee-lang.jar <source> <options>
```

Run without options
```
java -jar bee-lang.jar examples/Test.bee
```

Run with options
```
java -jar bee-lang.jar examples/Test.bee -a -n -c 4
```

## Options
```
-a                  Enable the alert comments
-n                  Enable the note comments
-c <Integer>        Set the number of cores to use
-v                  Print the language version
-h                  Print the options
```


## Documentation

- Call function with 0 args

```
@call function_name
```

- Call function with args 1, 2

```
@call function_name with 1, 2
```

- Call method from type c

```
@call c->function_name
```

- Call method from the same type

```
@call this->function_name
```

- Call method from the parent type

```
@call super->function_name
```

- Return with no value

```
#return
```

- Return with value

```
#return with 0
```

- Block

```
/*

*/
```

- Function with 0 parameters

```
#function function_name
/*

*/
```

- Function with parameters

```
#function function_name take x, y
/*

*/
```
- Call function in other thread and wait for the result

```
#let future be @run function_name
#let result be @wait future
```

- Loops

```
#loop 100
/*

*/

#loop x < 10
/*
    
*/
```


### Features
- Object Oriented Programming type, extend, constructor, this, super

```
#type Math
/*
    #function add
    /*
        #return with 3
    */
*/

#type Vector extend Math
/*
    #function init
    /*
        @call println with "Constructor"
    */

     #function add
     /*
        #return with 2
     */

    #function callCurrentAdd
    /*
        #return with @call this->add
    */

    #function callSuperAdd
    /*
        #return with @call super->add
    */
*/

#let v be @call Vector
@call println with (@call v->callCurrentAdd)
@call println with (@call v->callSuperAdd)
@set @call v.x be 1
@call println with @call v.x
```

- Functional Programming

```
#function printPlus take x, y
/*
    @call println with x + y
*/

#function callback take fun
/*
    @call fun with 1, 2
*/

@call callback with printPlus
```

- Run and wait keyword to run the function on other thread and return future so you can wait for it using wait keyword. 

```
#function show
/*
    @call println with @call threadName
*/

#let t be @run show
#let f be @wait t
```

- Load other file or all files in the directory!.

```
#load "Service.bee"
#load "Directory"
```

- Collections Data Strcture module include Array, LinkedList, Stack and generic functions work for all of them.

```
#let size be 5
/*
     #let array be @call createArray

     #let counter be 0
     #loop counter < size
     /*
         @call collectionAdd with array, ("Hello " + counter)
         @change counter be counter + 1
     */

     @call println with array
     @call println with (@call collectionGet with array, 2)
*/

/*
     #let stack be @call createStack
     #let counter be 0
     #loop counter < size
     /*
         @call collectionPush with stack, ("Hello " + counter)
         @change counter be counter + 1
     */

     @call println with stack
     @call println with (@call collectionGet with stack, 2)
*/
```

- Input and OS Modules

```
@call println with "Enter Your name :"
#let name be @call scanString

@call print with "Welcome " + name

@call sleep with 1000

@call println with @call osName
```

- Alerts is used to help you in the work you can enable/disable them from the CLI

```
# todo "Improve Performance"
# fix "Don't forget to fix this issue"
# warn "Remember that println return the size of the string :D"
```

- Notes is like print or logs but you can enable/disable them from the CLI

```
--Array & LinkedList Example.
--Stack Example
```

- Conditions if, loop
- Binary, Logical and Bitwise expressions support

- Group Expressions, You can group your expression inside ( )

```
(@call println with "Hello, World")
```

All the examples are on bee-lang/bin/exampes, I hope you will enjoy and have fun with this project
