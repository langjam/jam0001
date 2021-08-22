Name: BenJilks
Date: Sunday the 22nd of August 2021
Mood: good
-------------------------
Readme
-------------------------

         _____ ____ ____  _     ___ _______         __
        |_   _/ ___|  _ \| |   |_ _|_   _\ \   _   / /
          | || |  _| |_) | |    | |  | |  \ \ / \ / / 
          | || |_| |  __/| |___ | |  | |   \ V _ V /  
          |_| \____|_|   |_____|___| |_|    \_/ \_/   
        ----------------------------------------------
        The Greatest Programming Language in the World

The one and only programming language that you'll ever need. It 
solves every single programming dispute, in the most sensible way 
possible.

-------------------------

## Build and Run

You'll need `cmake`, `bison` and `flex` installed to build this 
project. Source files have the highly contended '.tgplitw' 
extension.

    mkdir build && cd build
    cmake .. && make
    ./tgplitw ../examples/helloworld.tgplitw

# Hello world

The first thing to note about this program, is that all code is 
followed by a C style comment. Anything that's not, is treated as 
a comment. That means this very readme file is executable, so 
give it a go! It encourages documentation, as all sources files 
are just documents with code in. As everyone knows it's far better 
to describe how code works in comments then to express it in the 
code its self.

Every statement is expressed as a 'pipeline'. They work very
similar to unix shell pipes. However, they always start with an 
expression and end with a variable to store the result into.
You can also put `/dev/null` to discard the output value 
(obviously).

    // "Hello, World!" -> print -> /dev/null

# Headers

All source files have to be wrapped in a header block. It has three 
sections, each one separated by a line of `'`s. The first contains
a list of attributes. Any number of these can be included, but a
name, date and mood at time of writing the file.

The second as a description of the files content, and finally the 
code itself.

    Name: BenJilks
    Date: Sunday the 22nd of August 2021
    Mood: good
    -------------------------
    Readme
    -------------------------

    ...

    _________________________

# Numbers

Numbers are very important in this language, as numbers and 
variables are the same thing. All numbers are expressed in base64,
where negative and decimals are allowed and `_`, `&` as the extra 2
digits. This means all variable names can be written as a number.
Assigning will change the value of that number from it's literal
to a new value.

The advantages of this system are obvious. Fortran did it, so it
must be good right?

    // "Literal: " -> print -> /dev/null
    // -aa.1 -> print -> /dev/null
    result is -650.016

    // "\nAssignment: " -> print -> /dev/null
    // 2 -> 1
    // 1 + 1 -> print -> /dev/null
    result is 4

    change 1 back to being 1
    // 3 - 2 -> 1

# Arrays

The age old debate, the eternal question, should arrays start at 
0 or 1? The real solution is neither of them, arrays should always
start in the middle.

There's also the question of trailing commas. What about proceeding
commas? That's what I've used.

    // "\nArrays: " -> print -> /dev/null
    // [,1 ,2 ,3] -> x
    // [,x[0] ,x[-1] ,x[1]] -> print -> /dev/null
    result is [2, 1, 3]

# Functions

I read somewhere that functions should only have one or two arguments,
so all functions only have one. It's also good to restrict types 
you can pass into a function, so you specify what types *can't* be 
passed in and returned. The function below can't have a None type
passed in or returned from it.

The variables (/numbers) `in` and `out` allow you to access function 
inputs and outputs.

As manners are important, you have to ask the interpreter to make a 
function with `please` and ending it with `thanks`. Making functions 
is a lot of work, so encouragement is needed.

    // please define none -> square -> none
    // {
            // in * in -> out
    // } thanks

    // "\nFunctions: " -> print -> /dev/null
    // 5 -> square -> print -> /dev/null
    result is 25

# Pipelines

Almost anything can be done in a pipeline, using the special pipes.
The first being `match`, I heard that this pattern matching is the 
new thing. So I decided to make all conditionals with it. I'm pretty 
sure they're just fancy if else statements right? Anyways, it takes 
the input and picks the first condition that *isn't* true.

The corresponding block is executed with `in` as the match statements
input and `out` being sent to the pipeline output.

    // "Enter number: " -> input -> to_int -> match
    // {
            // >5: { "a" -> out }
            // =None: { "b" -> out }
    // } -> print -> /dev/null

The other is `eval`, this just executes the following expression
and parses `in` as the pipeline input.

    // "\nEval: " -> print -> /dev/null
    // 5 -> square -> eval in + 1 -> print -> /dev/null
    result is 26

# Tee

A commonly used pattern is `tee`, this allows you to save a value in 
the middle of a pipeline.

    // "\nTee: " -> print -> /dev/null
    // [,b ,"x"] -> tee -> y
    // [,x ,y] -> print -> /dev/null
    result is [11, 11]

# Standard Library

Here are all the functions in the standard library

| Function Name | Description                                 | Input        | Output   |
|---------------|---------------------------------------------|--------------|----------|
| print         | Prints input on line                        | value        | None     |
| input         | Print input as prompt and ask user input    | prompt       | inputted |
| tee           | Tee off the input                           | [value, out] | value    |
| len           | Length of input                             | array        | length   |
| right         | Everything to the right of the index        | [arr, index] | arr      |
| left          | Everything to the left of the index         | [arr, index] | arr      |
| remove        | Everything except the value at index        | [arr, index] | arr      |
| random        | Generate random value                       | None         | random   |
| sleep         | Sleeps for x amount of seconds              | time         | None     |

-------------------------

# Some Examples

Just a few example scripts. All of these are also included as separate files under 'examples'.

## Sum

    There are no loops in TGPLITW, so recursion is the way to go!

    // please define none -> sum -> none
    // {
            // [,in ,"arr"] -> tee -> len -> eval in + [,"arr_len"] -> tee -> match
            // {
                    // =1: { [,arr ,0] -> remove -> sum -> eval arr[0] + in -> out }
                    // =None: { arr[0] -> out }
            // } -> out
    // } thanks

    // "\nSum: " -> print -> /dev/null
    // [,1 ,2 ,3] -> sum -> print -> /dev/null

## Seq

    // please define none -> seq -> none
    // {
            // [,in ,"count"] -> tee -> match
            // {
                    // =0: { count - 1 -> seq -> eval in + [,count] -> out }
                    // =None: { [,count] -> out }
            // } -> out
    // } thanks

    // "\nSeq: " -> print -> /dev/null
    // a -> seq -> print -> /dev/null
    // a -> seq -> len -> print -> /dev/null
    // a -> seq -> sum -> print -> /dev/null

## Shuffle

    As arrays are index from the middle, moving all middle element to the front makes 
    for an interesting shuffle function.

    // please define none -> shuffle -> none
    // {
            // [,in ,"arr"] -> tee -> len -> match
            // {
                    // =1: { [,arr ,0] -> remove -> shuffle -> eval arr[0] + in -> out }
                    // =None: { [,arr[0]] -> out }
            // } -> out
    // } thanks

    // "\nShuffle: " -> print -> /dev/null
    // a -> seq -> shuffle -> shuffle -> print -> /dev/null

## Map

    Functions can be used as normal objects, so a map function like this is possible.

    // please define none -> map -> none
    // {
            // in[-1] -> func
            // in[0] -> arr

            // arr -> len -> eval 0 - in / 2 -> index
            // arr[index] -> func -> value

            // arr -> len -> match
            // {
                    // =1:
                    // { 
                            // [,arr ,index] -> remove -> rest
                            // [,func ,rest] -> map -> eval [,value] + in -> out
                    // }
                    // =None: { [,value] -> out }
            // } -> out
    // } thanks

    // "\nMap: " -> print -> /dev/null
    // a -> seq -> eval [,square ,in] -> map -> print -> /dev/null

## Nested

    Functions can also be nested inside other function. Everything is scoped.

    // please define none -> nested -> none
    // {
            // please define none -> test -> none
            // {
                    // in + 1 -> out
            // } thanks
            // in -> test -> out
    // } thanks

    // "\nNested: " -> print -> /dev/null
    // a -> nested -> print -> /dev/null

## Binary Search

    As arrays start from the middle, we can use this to make a binary search with

    // please define none -> search -> none
    // {
            // in[-1] -> arr
            // in[0] -> key

            // arr[0][-1] -> match
            // {
                    // >=key: { [,arr ,0] -> right -> eval [,in ,key] -> search -> out }
                    // <=key: { [,arr ,0] -> left -> eval [,in ,key] -> search -> out }
                    // =None: { arr[0][0] -> out }
            // } -> out
    // } thanks

    Define some test data
    // [
            // ,[,4 ,"a"]
            // ,[,6 ,"b"]
            // ,[,7 ,"c"]
            // ,[,a ,"d"]
            // ,[,G ,"e"]
    // ] -> x

    Run a search
    // "\nBinary Search:" -> print -> /dev/null
    // [,x ,G] -> search -> print -> /dev/null

