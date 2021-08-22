# Vrv 

A #LangJam-2021, comments-first programming language

In vrv comments operate over code. Just like in [concatenative](http://concatenative.org) languages words operate over data on a stack, 
in vrv you have comments operating on their surrounding program syntax.

## Dependencies

Vrv is implemented in Scala, thus you will need a working [Java](https://java.com) runtime on your system.
Be sure you can execute the `java -version` command successfully.

You will also need the [coursier](https://get-coursier.io/docs/cli-installation) CLI in order to build this
project.

If you already have Java, you can get coursier by doing:

``` shell
curl -fLo coursier https://git.io/coursier-cli && chmod +x coursier
```

Be sure to place the `coursier` executable in this project's directory, and that
you can execute the `./coursier --version` command successfully.


## Running

Use `./vrv run FILE` to execute a vrv file. For example:

`./vrv run samples/sashimi.vrv`

The `./vrv` executable uses `./coursier` to launch `./vrv.sc` via the
[Ammonite](https://ammonite.io/#Ammonite-REPL) runtime.

``` sh
./vrv run ./samples/hello-world.vrv # All tutorials end with hello world, now you are an vrv expert!

./vrv run ./samples/importing.vrv # A file loading another vrv resource.

./vrv parse ./samples/nested.vrv # parse and show the AST
```

## Vrv source programs

The source of a vrv program is composed only of comments and data.

Comments are c-style. `//` for single-line comments and `/* */` for multi-line nestable comments.

```scala
this is data
// this is a comment
more data again
```

Traditional programming languages completely ignore comments in their source code, 
vrv sees comments as the only first-class programming construct.

Any c-style source file like the following Scala code is also valid syntax for a vrv program:

``` scala
package baz

// hello world
object Bar {
  // hola mundo
  val a = 22
}
```

In the previous snippet everything is data, except for the `hello world` and `hola mundo` comments which are only 
thing that vrv cares about.

## Execution model

All things outside of comments in vrv are just considered data, and comments are the program driving force. 
Both data and comments are simply chunks of symbols -tokens separated by space and new-lines read from the source file-.

vrv is a symbolic language, meaning that it operates over symbols without explicitly
requiring these symbols to be bound to any value or function.

_The result of all vrv programs is a single comment_.

program execution is done via pattern matching comments with the current source file.

similar to how many concatenative languages have a data-stack to operate upon, in
vrv *comments* operate on its left and right surroundings of code-as-data.

To help understand it let's inspect a tiny vrv program:

``` scala
a b // c d
e f
```

Imagine the previous program as `List(a b) Comment(c d) List(e f)`, if there exists a
definition for `c d` it will take both lists of code surrounding it in order to produce 
a potentially different source code, perhaps adding more comments, or changing its surrounding code.

An example of this is the builtin `load` builtin which inlines the imported AST from another vrv file
at the very position where the comment exists. Try running `samples/importing.vrv` and `samples/sashimi.vrv`

#### Evaluation order for first-class comments

In vrv, comments that have no nested comments within them are called `first-class`. Once encountered
by the interpreter, only first-class comments get evaluated, and comments having nested comments are
recursively walked trying to find its inner first-class comments.

In the following example the first evaluation happens at the `c` comment. `c`'s implementation could 
potentially change the whole source file. and after that (if they still exist) `f g` and `i j` would be the
next candidates for execution.

``` scala
a b // c
d /* 
   e // f g
   h /* i j */ k
   */
l m
```


