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
