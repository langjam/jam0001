# D-script

My idea for the theme of "first-class comments" was to make the language where the instructions are described by the comments.
I'm not sure if I hit the mark there or not but I'm fairly pleased with how it came out. The interpreter _is_ however
being held together by sheer force of will and probably break at some light prodding. The examples run fine though.

## Overview

Because the goal of this language was to be as close to plain English as possible, the syntax should be pretty intuitive. A statement like `function foo takes bar prints bar with newline and returns nothing` is valid and defines a function named `foo` that has one parameter, `bar`.

## Build instructions

There are three example files in `./examples`

`cargo run -- ./examples/example1.d` will run the first, the other two are named in kind.
