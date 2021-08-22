# Mark Sideways

MarkSideways is a programming language that uses MarkDown documents as its code file format.

This programming language was created as part of [LangJam](https://github.com/langjam/langjam). It is a solo project by [Blake O'Hare](https://github.com/blakeohare).

The overall structure of your program is determined by markdown structure such as headers and bulleted lists. The code is written in markdown triple-backtick (```) code blocks. Basically your code is one giant comment with bits of executable code specifially marked.

In fact, this entire `README.md` file is a Hello World example. You can run this document by running the following command:

> `$ python ms.py README.md`

![](preview.jpg)

**How it works**

All markdown text is a comment unless otherwise stated. The top-level header (i.e. the `#` header at the top) is the title of the program. Classes are defined by the existence of 2nd-level headers (i.e. the `##` headers). Methods are defined by 3rd-level headers (i.e. `###` prefixed headers). When full-block code snippet appear, the header they are listed under determines where they belong.

- Code snippets directly below the main header will be included as the initial execution, kind of like a `main()` function in most languages.
- Code snippets located directly under a 2nd-level header are part of the class' constructor.
- Code snippets located directly under a 3rd-level header are part of that method.
- Arguments are defined as bulleted lists with an inline code snippet of the argument name as its first text in the bulleted list item. The argument is associated with whichever entity it falls below.

Because this code is still part of the main header ("How it works" is just bold text, not a header) any code blocks listed here will run. There is a header below called "Example Code" and so that implies the existence of a class called ExampleCode. We can instantiate the class with the method `.init()`. We do so here...

```
ExampleCode.init();
```

## Example Code

This section of the document is the constructor of the Example Code class (class names, variables, and fields are case/space/punctuation insensitive to allow for natural language in the document comments, but consitent parsing in the code itself).

This constructor contains a single line:

```
print("Hello, World!");
```

## Other Demos

There are a variety of demo code files in the `samples/` directory. You can run any of these samples by passing them to `ms.py` as-is. Remember, markdown **IS** the file format for marksideways. These are not just documentation readmes of these program. They **ARE** the programs.

- [Tetris](/samples/Tetris.md) - 