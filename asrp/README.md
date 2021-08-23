# Stickynotes - a Smalltalk-like programming language with first-class comments.

*Add some sticky notes to your variables.*

## Objects

Stickynotes is a Smalltalk-like language where everything is done through method calls (but the syntax is closer to Python). Builtin functions like `print` can be thought of as calls to a global object `W`. So these two calls are equivalent.

    print("Hello world\n")
    W.print("Hello world\n")

The root object is `Obj` (or equivalently `W.Obj`). To create a new object, copy an existing one.

    MyObj = Obj.copy("MyObj")

Currently the new object inherits all methods of the parent *at the time of the copy*. Adding new method to the parent will not add them to the child.

For instances, pass the same name to copy and for subclasses, pass a new name.

    obj1 = MyObj.copy("MyObj")
    obj2 = MyObj.copy("MyObj")
    obj3 = MyObj.copy("MyObj")
    MySubObj = MyObj.copy("MySubObj")

Methods are added via assignment

    MyObj.myMethod = [ self |
      print("Called myMethod\n")
      self.doSomeOtherStuff()
      ]

There's no distinction between methods and attributes

    MyObj.foo = "foo"

All method must accept self as last argument and it has to be named `self`, even if unused.

    True.ifTrue = [ block self | block.eval() ]

## Comments

Comments are first-class. They can be bound to variables

    myComment = /* This is a comment */

and passed around as arguments

    print(/* This is a comment argument */)

Comments can be added to any object

    myRedObj = MyObj.copy("MyObj") /* This object is red */
    myRedObj.col = "red"

and retrieved from the `.comment` attribute

    print(myRedObj.comment) /* This will print "This object is red" */

Multiple comments are appended to each other.

    commentPrinter = [ obj |
      print(obj.comment)
      ]
    commentPrinter(myRedObj /* and is passed to the printer */)

This will print `This object is red, and is passed to the printer`.

### Comment usage

Comments can be thought of as sticky notes added to see what happened to the handling of the object.

This helps troubleshoot broken states, especially from mutations, that do not explicitly produce an error.

## Displaying comments

Displaying comments can be turned on or off with

    show_comment = True
    show_comment = False

For example

    showTest = [ |
      show_comment = True
      /* This comment will show */
      show_comment = False
      /* This comment will not */
      ]

    showTest()

See [examples/show_comment.sn](examples/show_comment.sn).

In the future, we'd like to add a custom callback function to make filtering and other operations possible.

## Blocks

Blocks

    [ foo bar |
      print(foo)
      print(bar)
      print("\n")
      ]

are anonymous functions (lambdas). The argument names are between `[` and `|` and with the body afterward. `|` is needed even if the function is nullary

Blocks are called with `block.eval`.

    [ | print("Hello world\n") ].eval()

### Language syntax

See [snotes.grammar](snotes.grammar) for the full grammar describing the language.

## Internals

Stickynotes is actually a concatenative language (like Forth) but relies heavily on the local environment rather than stack shuffling. However, this means there is no arity checking and can be polluted by unused return values (`discard()` them explicitly to avoid this).

### Builtin functions

The initial built in globals are

    `attrset`, `attrget`, `assign`, `return`, `print`, `discard`, `Obj`, `W`, `Void`, `void_obj`, `Block`, `bp`, `breakpoint`

## Running

### Dependent library

[pymetaterp](https:/github.com/asrp/pymetaterp) is used for parsing and is bundled here. pymetaterp itself has no dependencies.

### Running

    python snotes.py lib.sn your_script1.sn your_script2.sn ...

`lib.sn` contains some useful functions defined at runtime, such as if statements and while loops.

### Examples

See the [examples](examples) directory for many examples.

    python snotes.py lib.sn examples/hello.sn

## Project stats

```
  398 snotes.py
   31 snotes.grammar
   17 lib.sn
  446 subtotal

  816 pymetaterp
 1262 total
```
