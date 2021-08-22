# bubblegum

A mouthful of bubblegum-flavored-github-flavored-markdown.

## What is bubblegum?

Bubblegum lets markdown authors embed code inside comments. These code snippets can be called in a REPL to modify the 
markdown document on disk. Bubblegum targets github-flavored-markdown and is fully interoperable with existing 
parsers/renderers.

### Markdown has comments?

Kind of...

There is no formal specification for embedding comments in markdown. However, markdown authors will typically
use one of two methods when compelled to embed un-rendered text inside their documents: HTML-style comments, and link 
references. Bubblegum uses the latter for function definitions and element tagging. 

## Element tagging

In bubblegum, the discrete elements of a markdown document can be tagged for reference at a later time. The following
markdown snippet tags a checklist with the name "TodoList":
```markdown
# Things I have come here to do
[TodoList]::
*   [ ] Chew bubblegum
*   [ ] Kick butt
```

Any root-level element can be tagged (even other tags!). Tags must have unique names.

## Functions

After an element has been tagged, functions can now be defined as members of that tag. Because of the limited (zero) use of
parenthesis in this style of commenting, the syntax within bubblegum functions is a Smalltalk-influenced language
which we are calling "mouthful".

Building off of the last snippet:
```markdown
[TodoList]:: (
    completeKickButt
        this CheckItem: 'Kick butt'
)
```

Once a function has been defined and associated with a tag, that function can now be called from the REPL:

```smalltalk
TodoList kickAss
```

## REPL

This repo hosts a TypeScript project which sets up a bubblegum REPL and handles all I/O and parsing necessary to manipulate
markdown files with bubblegum. To get started, clone this repo before installing dependencies and running the start script:

```shell
$ npm install
$ npm run start demo.md
```

## Development

```shell
$ npm install --dev
$ npm run dev # run without building
$ npm run build # build to js
```

## Mouthful Language & Builtins

Many JavaScript features are already accessible, you should be able to 1-to-1 translate our Smalltalk-like Mouthful language to JavaScript.

e.g. `console log: 'Hello, World!'`

Most syntax follows smalltalk as close as possible. However, there are a few major departures. Such as:

Arrays are written as: `@{ 1 2 3 4 5 }`

Strings with single quotes: `'this is a string'`

And a grouped expression as: `${4 + 4}`, not with parenthesis like many other languages.

Here are some additions to overcome some things that can't be 1-to-1 translated:

To access an index on an Array, use:

`Array at: index`,

e.g. `@{ 10 11 12 } at: 2` ==> `12`

To get a range of numbers (not inclusive), use: `Number to: end`,

e.g. `0 to: 10` ==> `@{ 0 1 2 3 4 5 6 7 8 9 }

The not operator for boolean values is: `Boolean not`,

e.g. `true not` ==> `false`

An if is written as: `Boolean ifTrue: then`,

e.g. `1 < 10 ifTrue: [ console log: 'True!' ]` ==> True!

You can also use: `Boolean ifFalse: then`,

e.g. `10 < 1 ifFalse: [ console log: 'False!' ]` ==> False!

The two can be combined like: `Boolean ifTrue: trueBranch ifFalse: falseBranch`,

e.g. `1 < 10 ifTrue: [ console log: 'True!' ] ifFalse: [ console log: 'False!' ]` ==> True!

# Library

Bubblegum comes with some handy functions for basic tasks and manipulating markdown elements.

### Blockquote

The `append` function will append some markdown to the end of the element.
```smalltalk
MyBlockquote append: 'some **text**'
```
 The `text` function can be used to either set the text for the contents of the element, or to just return the contents.
```smalltalk
MyBlockquote text: 'some **text**'
MyBlockquote text
```

### Code

The `append` function will append some markdown to the end of the element.
```smalltalk
MyCode append: 'some **text**'
```
The `text` function can be used to either set the text for the contents of the element, or to just return the contents.
```smalltalk
MyCode text: 'some **text**'
MyCode text
```
The `language` function can be used to either set the language for the code block, or to just return the language.
```smalltalk
MyCode language: 'js'
MyCode language
```
The `eval` function will eval the contents of the code block and return the result if the code block language is set to 'js'.
```smalltalk
MyCode eval
```

### Heading

The `append` function will append some markdown to the end of the element.
```smalltalk
MyHeading append: 'some **text**'
```
The `text` function can be used to either set the text for the contents of the element, or to just return the contents.
```smalltalk
MyHeading text: 'some **text**'
MyHeading text
```
The `depth` function can be used to either set the heading depth, or to just return the depth. If setting the depth,
the desired depth should be an integer from 1-6 (h1 - h6).
```smalltalk
MyHeading depth: 1
MyHeading depth
```

### List

The `checkItem` function will check the first unchecked item in the list whose text matches the argument.
```smalltalk
MyList checkItem: 'some **item**'
```
The `uncheckItem` function will uncheck the first checked item in the list whose text matches the argument.
```smalltalk
MyList uncheckItem: 'some **item**'
```
The `addItem` function will add an item to the list with the given text.
```smalltalk
MyList addItem: 'some **item**'
```
The `removeItem` function will remove the first item from the list whose text matches the argument.
```smalltalk
MyList removeItem: 'some **item**'
```
The `removeItemAtIndex` function will remove the item at the given index from the list.
```smalltalk
MyList removeItemAtIndex: 0
```
The `isChecked` function will return if the first item whose text matches the argument and is also a checkbox item is
checked. If there are no items in the list matching these criteria then the function will return `undefined`.
```smalltalk
MyList isChecked: 'some **item**'
```

### Paragraph

The `append` function will append some markdown to the end of the element.
```smalltalk
MyParagraph append: 'some **text**'
```
The `text` function can be used to either set the text for the contents of the element, or to just return the contents.
```smalltalk
MyParagraph text: 'some **text**'
MyParagraph text
```
