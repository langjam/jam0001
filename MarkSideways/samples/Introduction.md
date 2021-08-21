# MarkSideways Example Code

This is not *just* a markdown file that includes code snippets. **This 
markdown file is the actual code file consumed by MarkSideways.**

To execute this file, run `python ms.py test.md` on the command line.

Most programming languages have files with code and there are comments
included throughout the code to make it more readable. However, in
MarkSideways, markdown files are the actual code document. Code snippets are
included within the markdown using ``` and their purpose and context are
defined by the overall structure of the markdown file itself, through the
use of headings, sub-headings, and bulleted lists.

The main title of the document is the name of the program. Any block-style 
code snippets included under the main header is where execution of the program
begins. For example, this is a simple line of code that displays "Hello, World!":

```
print("Hello, World!");
```

If there are additional code blocks directly under the main header, these
are also included within the program's initial execution code. You can think
of this as the `main()` function that would appear in other programming
languages.

After printing `Hello, World!`, this program continues on demonstrate how
Objects are instantiated. The implementation of these methods demonstrate
how variables, methods, and loops work and can be seen in later sections
of this document.

```
example = CountTo10Example.init(1, 10);
example.LoopExample();
```

## Count To 10 Example

Classes are defined by markdown headers. The name of this class 
is `Count to 10 Example`. When using classes, the name of the class is
case, space, and punctuation insensitive to allow for readable headers
but unambiguous and traditional syntax when instantiating a class.

The constructor of classes is denoted by any code block snippets that
appear directly under a heading that aren't also under a sub-heading.

Additionally, any arguments for the constructor are included in a bulleted
list that begins with an inline code snippet of the name of the argument.
The constructor for `CountTo10Example` takes two arguments: `begin` and `end`...

- `begin` -- where to start counting
- `end` -- where the counting stops (inclusive)

Now that we have our arguments defined, we can use them in the constructor
itself:

```
this.begin = begin;
this.end = end;
```

Like many object-oriented languages, a reference to the current class instance
is done by using the `this` keyword.

As with the `main()` execution area, any consecutive code snippets that appear
under a header that are also not part of a sub-header will be included in the
constructor's body of code. After assigning the arguments to their equivalent 
instance fields, we can print out a message announcing our intent to count:

```
print("Getting ready to count from " + begin + " to " + end + "!");
```

Again, all this is part of the class' constructor. We now move on to the
class' methods. 

### Loop example

Methods on a class are defined by sub headers below the class definition header.
For example, this method is called `LoopExample` and is a member of the 
`CountTo10Example` class.

Because this method uses no arguments, there is no bulleted list here. If we did
want to include arguments in our method definition, they would appear as a bulleted
list in much the same way as the constructor above. Instead of arguments, we 
use the instance fields `begin` and `end` to loop through these numbers.

A for loop is constructed by using the word `for` followed by a variable name
followed by a `=` character, followed by start and end points. The end point 
can be inclusive if you use the word `thru` or it can be exclusive if you use the 
word `till`. In this case we want to include the value in `this.end` in our
counting, so we use `thru`.

```
for i = this.begin thru this.end {
  print(i + " Mississippi");
}

this.ClosingMessage();
```

The last line of code is a method call to another method that we haven't 
defined yet called `ClosingMessage` which will be defined in the next section.

### Closing Message

This method displays a brief message to show after the counting has completed.

```
print("Ready or not, here I come!");
```
