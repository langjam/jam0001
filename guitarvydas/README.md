# Synopsis

Transpile diagrams to code.

## Introduction

To me, 1st class comments means that:

- if you change the comments, the execution (code) changes.

The following contains a possible solution and a discussion about the [language jam](https://github.com/langjam/langjam).

[_I find that writing about a problem helps me think about the problem. The following may read as a stream of consciousness._]

I contend that *diagrams* are just as readable as *textual comments*, yet, it is possible to compile diagrams to code, something which is not usually done with *textual comments*.

I show how to compile diagrams to running code.

# Build and Run
- to see the source code, open `sequence.drawio` and `details.drawio`

- yes, the source code is in the form of diagrams (see Discussion section)<img src="https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/sequence.png?raw=true" alt="sequence.png" style="zoom:67%;" />

  

  <img src="https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/details.png?raw=true" alt="details.png" style="zoom:67%;" />

- `sequence.drawio` shows the order in which components must be executed

- `details.drawio` shows the details within the components (BASH at the moment)

- run `run.bash` from the command line, it should print 
```
hello
... from process A
... from process B
goodbye
```

## Prerequisites

- npm install ohm-js
- npm install atob
- npm install pako
- bash, node.js, tsort (a *NIX command)

---

# Abstract

Transpile drawings to code.

The main purposes of this project are:

1. to riff on the idea of 1st-class comments (as per the langjam theme)
2. to show that it is easy to transpile diagrams to code.

For simplicity of explanation, only a toy example is transpiled.

Components on the drawings are *concurrent* by default.

Four transpilers (languages) are created:
1. sequencing diagram -> JSON
2. details diagram -> JSON
3. sequence.json -> BASH
4. details.json -> BASH

This is done using drawio, PROLOG, Ohm-JS and JavaScript.

Each transpiler consists of a pipeline of smaller transpilers.

# Key Takeaways
- multiple views on one problem
- one mini-language (SCN[^scn]) for each problem
- separation of concerns
- isolation
- concurrency
- simplicity
- the New Assembler.

[^scn]: SCN means Solution Centric Notation.

# Key Technologies
- drawings (.drawio, .svg in the future)
- exhaustive search
- PEG
- JSON
- Ohm-JS (PEG parser)
- Glue tool (code emitter/formatter for Ohm-JS)
- pipelines.

# Drawings

.Drawio files are _groked_ and _transpiled_ for this project.

.SVG should be another easy-to-grok drawing format (rectangles, ellipses, text, lines).

## Exhaustive Search

I used PROLOG (SWIPL) for this project, but, there are other[^1] choices, e.g.
- miniKanren
- datalog
- core.logic
- nested loops
- prolog.js
- etc.

[^1]: I've tried only some of these choices.

# PEG
PEG means Parsing Expression Grammars.

Many PEG libraries exist for many languages.

I currently think that Ohm-JS, and especially the Ohm Editor, are the best-of-breed in PEG technologies.

# Separation of Concerns

## Multiple Views

I use multiple paradigms for this problem. 

Each *paradigm* is a *view*.

Each paradigm (view) gets its own syntax (I call this syntax an *SCN* - like a mini-DSL).

## Ohm-JS

Ohm-JS separates concerns into
- grammar
- semantics (understanding and emitting).

Most PEG libraries conflate grammar+semantics, e.g. by annotating the grammar with variable names. 

Ohm-JS does this differently.

# Isolation

Build and forget.

Heavy use of regression testing is a _tell_ that isolation is not present.  

[_If new code impacts existing code, the primary problem is lack-of-isolation, not lack of testing._]

# Concurrency

It is not reasonable to build isolated components unless they are concurrent-by-default.

Synchrony is a hidden form of dependency.

Dependency is anti-isolation.

# JSON

I use JSON as an intermediate file format.

JSON has problems, but, JSON is ubiquitous.

# Simplicity

The goal is to make this project _so simple_ that it evokes the _this is not hard enough_ and _this is not powerful enough_ and _this is cheating_ gag reflexes.

We should not be wasting brain power on the fine details required by most GPLs. 

We should devote our brain power to more interesting, higher-level concepts.

# The New Assembler

I use GPLs (general purpose languages), in this case JavaScript and JSON and PROLOG, as _assembly_ languages.

The less syntax and the less type checking (!) the better (see, also, the Readability section below).

Lisp (Common Lisp, Racket) would be my choice, but Ohm-JS has not yet been ported to lisp.

80x86 is Old Assembler.  JS and Lisp are the New Assemblers[^python][^ts][^wasm].

[^python]: Python would be included in this group of  _new assemblers_, except that Python has too much syntax (indentation) that makes it (a bit) harder to transpile to python.  What is needed is a Python variant that uses nested braces instead of indentation.
[^ts]: Typescript is exactly the wrong idea for a _new assembler_.  Types are only useful for human readability (writability), but _new assemblers_ should accomodate machine-readability (and writability), not human-readability.
[^wasm]: See TS footnote.  Adding types to _assembler_ makes it harder to emit automatically. Use PEG parsers to add syntax skins and type skins to _assemblers_.

# Pipelines

This project consists of many transpilers (aka languages), but each transpiler is very simple and does exactly one thing (well (or at least as well as could be accomodated in 48 hours :-)).

PEG makes it possible to quickly build many transpilers for one project.   

# Discussion
To me, 1st class comments means that:
- if you change the comments, the execution changes.

This means that comments need to compile to code.

In my opinion, this means a separation between:
- DI[^DI]
- Implementation.

DI must result in Implementation.

[^DI]:  DI means Design Intent. Sometimes DI is labelled as Software Architecture.

On the first cut of this project, I want DI to generate Implementation.  DI diagrams to Implementation code.

If Implementation is manually tweaked, then there should be some "providence" that shows how the DI was transmogrified into the Implementation code.

Why would anyone further tweak Implementation? 
- Optimization
- Testability
- Security
- etc., etc.

Is tweaking the same as "Round Trip"?
- No, Round Trip is hard.
- No, Round Trip usually means that anyone can tweak the Implementation to their hearts' content. In Structural Engineering, this does not happen, it's one-way only. The Implementors (Construction Workers) don't get to change the blueprints. It's one-way only. If a design change is required, the Implementor goes back to the Engineer and, if necessary, the Engineer goes back to the Architect. The blueprint gets modified by the signatory (usually an Engineer) and the Implementors must build according to the blueprint specification. DI'ers (Architects and Engineers) get paid big bucks, but are liable - in Law - for their designs. [Certified Engineers can be sued if their design fails.  It is _illegal_ to call yourself an Engineer unless you've been certified.].

Note that git, github, diff, etc. should help with provinence of DI. Instead, they are used for provinence of Implementation (code).

I see at least two ways to make languages for DI:
1. Cast the code as markdown.  Generate code from the .md file(s).
2. Cast the code as SVG diagrams, generate code from the diagrams.

I've done both of the above.

I'm going to start simple, for this langjam, and work my way up.

# Bare Essentials
What are the _bare essentials_ for making DI languages for human use?
- concurrency
- isolation.

## Concurrency
Concurrency is necessary, because that is what normal humans are used to.  

For example, we teach 5 year-olds hard realtime and we use a several-hundred year old notation for hard realtime. 

Piano lessons.

For another example, when we cook dinner, we follow a concurrent specification, called a _recipe_ ("while the potatoes cook, chop blah blah blah").

Normal concurrent solutions often fail when we try to scale them up. Spreadsheets work up-to-a-point, then collapse into a big mess. We don't need to switch to thinking synchronously to solve the scaling-up problem, but most programmers are taught this way of doing things and most programming languages are synchronous at their core (thread libraries are a symptom of *anti*-concurrency).

Programmers have concurrency beat out of them in university. A lot of accidental complexity has been caused by trying to use synchronous languages for asynchronous problems.

## Isolation
Isolation means 

- *Build and Forget*.

Tweaking a component must not not affect how other components work. 

Developing a new component must not affect how existing components work. 

Eschew dependency.

UNIX does this, but UNIX's kind of isolation has been overlooked. Even a lowly C program is isolated when run under UNIX. 
- It doesn't matter if the C program has memory leaks or not ; when the process dies, UNIX cleans up after it.
- It doesn't matter if the program manipulates State or not. Programs are black boxes, we don't know (don't care) how they are implemented.  If a program runs too slow, give it to a Production Engineer. If a program is hard to maintain, give it to a Maintenance Engineer.

## Business Org

Businesses are arranged in a hierarchy with no sideways dependencies.

Each business unit is isolated from other business units.

Managers of business units report to their managers. 

VPs might oversee some 100's of workers, but the lines of communication are restricted. Every VP communicates (downwards) to a small number of department managers. Every VP reports to the CEO (upwards communication).  The CEO does not need to micro-manage each department because the departments are isolated and the department managers look after the details.

There is even a term, in business, for when workers break the organizational hierarchy. It's called "Going over your boss's head".

# Efficiency
In DI, efficiency doesn't matter much. The only thing that matters is if the DI design can be run "in a reasonable manner" by the DI Architect (and Engineer).

# Staring Small
## How?
Cheat as much as possible.

# Actual Problem
At this very moment, I am working on something that uses a _bash_ script to build.

The _bash_ script contains a lot of detail.

If I were tasked with maintaining this project, the first thing that I would want to know is "what is the bare essence of what this project accomplishes?".

I don't trust text comments, so, as a maintainer, I would skip over the text comments and look at the code.

The code contains a lot of details and I need to build up a model in my head of what is going on.  The field of _design recovery_ researches this kind of issue.

At a very simple level, the first thing I would do is draw an ad-hoc diagram of the major steps in this script. I would draw the diagram on a whiteboard, or, if working alone, I would draw the diagrams on a piece of paper.

## The Bash Script

[_Don't try to understand this script, read the commentary below._]

```
#!/bin/bash
clear
set -e
trap 'catch' ERR

catch () {
    echo '*** fatal error in run.bash'
    exit 1
}

cp ../phase1/out.pl fb.pl

#######

# drawing agnostic operations

# rects
swipl -q \
      -g 'consult(fb).' \
      -g 'consult(rects).' \
      -g 'printRects.' \
      -g 'halt.' \
    | ./augment-fb

# bounding boxes
swipl -q \
      -g 'consult("../phase1/out.pl").' \
      -g 'consult(boundingBoxes).' \
      -g 'printBB.' \
      -g 'halt.' \
    | ./augment-fb


#######

```

The _really_ important parts are the two comments:

- rects
- bounding boxes

but, there are about 33 lines in the script.  A maintainer has to wade through all of the detail to figure out what is going on.

[_Note, too, that I abbreviated the 2 comments, because I was running out of patience. Maybe if I had executable comments, I would have been more verbose._]

This script is essentially a _toy example_.  I chose something simple to help illustrate my main points (1st class comments) and to make the early going easy, for this jam.

[_Again, you don't need to understand why I wrote these 2 lines to understand the techniques I describe in this essay._]

[_Late addition: The example script has been reduced._]

## Desired Script

What I really want is something like...

<img src="https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/2021-08-20-jam0001-top%20level.png?raw=true" alt="2021-08-20-jam0001-top level.png" style="zoom:67%;" />

## Making It Simpler

I have less than 24 hours to build this.

What can I chop to make this simpler?

The boxes represent at least 2 views of the solution:

1. sequencing of operations
2. details.

Let's start by making separate diagrams for each of the view.

## Sequence Diagram

<img src="https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/sequence.png?raw=true" alt="sequence.png" style="zoom:67%;" />

## Details Diagram

<img src="https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/details.png?raw=true" alt="details.png" style="zoom:67%;" />

"Red" boxes mean _synchronous_ code.  

The default is asynchronous code.

# Emitters 3 & 4

Emitters 3 and 4 create parts of the final _bash_ script from the _.json_ files.

## Create Basic Grammar

The output from Drawio is XML, and is similar to SVG.

The Drawio format is called *mxGraph*.

To begin, I built a basic grammar for this format.  

I tested the grammar using the [Ohm Editor](https://ohmlang.github.io/editor/).

- basic.ohm
- use ohm editor to test/develop against sequence.json and details.json

## Create Identity Emitter

This is a step that reduces development time.

An _identity_ emitter parses its input and outputs it as-is.

Most compiler technologies - scanners and parsers - destroy information along the way.  They destroy spaces and newlines and *tokenize* the input.  Using such technologies, it is almost impossible to build an identity emitter.  

Worse, it is almost *unimaginable* to build an identity emitter.

Ohm-JS uses two kinds of rules

1. The standard PEG rules (called _lexical_ rules)
2. An extension of PEG, which skips whitespace (called _syntactic_ rules).

_Syntactic_ rules make writing grammars easier (less noise, no need to worry about whitespace).  _Identity_ grammars built using _syntactic_ rules do not preserve whitespace, so some whitespace needs to be added back into the parsed code. _Syntactic_ rules do not *tokenize* the input, like most older compiler technologies.  _Syntactic_ rules can still produce *identity* grammars.  A true identity grammar would use _syntactic_ rules only. This is not necessary.

Creating an *identity* grammar is only the first step in creating a transpiler.  Once an *identity* grammar has been created, it becomes easy to hack on the *identity* grammar to produce useful transpilers.

The original identity emitter was built in HTML.  See files:

- identity.glue
- basic.html
## Overview of the Project

The project consists of a pipeline of transpilers.

Each transpiler is built in 2 parts

- *grok* (the grammar)
- *emit* (the code that "does something" with the matches from the *grok* phase) ; the *emit* code is built using the *glue* tool (Ohm-JS calls this code the *semantics* and expects programmers to write it manually (in JavaScript).  Instead, I built and use the *glue* tool to create the JavaScript for me.  *Glue* itself is built using Ohm-JS).

The phases (parts) of this project are:

1. Input raw, compressed, .drawio format and uncompress it into human-readable XML (mxGraph)
2. Expand all "style=..." into separate attributes.  The mxGraph code contains HTML (XML) elements containing *attributes* and *bodies*.  The syntax of the attributes is inconsistent, some attributes are written explicitly, while others ("style") are lumped together into one string.  This 2nd phase normalizes all attributes to be explicit.
3. Attribute Elider.  Most of the attributes pertain to the graphical editor.  This phase discards many attributes, leaving only those attributes which are needed by the transpiler.
4. Symbol Table.  Drawio creates unique names for each element, but, the names are long and not human-readable.  This phase creates a mapping from Drawio long names to short names, such as "id8". [_This mapping is not strictly necessary, but is useful in bootstrapping the transpilers._]
5. Emit.  This phase is based on the input grammar and produces output, e.g. using JS *template* strings.  The *emit* phase is slightly different for each of the four transpilers.
6. Sort. Transpilers 1 and 2 produce facts[^fact] for a factbase in PROLOG format. PROLOG requires that PROLOG rules (and facts) be contiguous in the source code. *Sort* is sufficient to produce such grouping.  This phase is quite simple - its input consists of triples (one triple per line) and its output consists of the same triples ordered alphabetically.
7. Gotchas. The input source code may contain spaces and characters that are not allowed in JSON code.  Illegal characters are mapped to legal characters using very simple search-and-replace code. 
8. Gotchas II. Transpilers that write transpilers often have problems representing simple string sequences such as newlines.  Most string escape sequences are good for only one level of transpilation.  If the transpiler wants to insert escape sequences containing escape sequences, then some kind of mapping needs to be employed.  In some cases (determined by trial and error) these problems are fixed by searching-and-replacing the string sequences by "magic" characters during the phases of the transpiler and then expanded back to their original form for final output.  In this project, the details transpiler maps newlines to "~~" during processing. [_Note that the mapping is one-to-many_].  Dashes in the input source are mapped to double-underscores.

[^fact]: A fact is a simple triple: { relation, subject, object }, written in PROLOG as `relation(subject,object).`.  Triples can be easily represent as s-exprs in Lisp and as user-defined data structures in many GPLs.  Note that more interesting data structures can be devolved into triples, where _pointers_ are represented as explicit triples.

### Transpiler 1 - Sequence Grok

The first transpiler converts sequence.drawio into a JSON factbase (sequence.json).

### Transpiler 2 - Details Grok

The second transpiler converts details.drawio into a JSON factbase (details.json).

### Transpiler 3 - Sequence Emitter

The 3rd transpiler inputs .json code (sequence.json) and produces a file which contains pairs of component names, suitable for the *tsort* (topological sort) command (Unix, Linux).

### Transpiler 4 - Details Emitter

The 4th transpiler inputs .json code (details.json) and produces a file of _bash_ functions.

This phase creates _dead code_ by emitting _bash_ functions that are not used.

_Dead Code_ is not really an issue, but can be removed automatically at a later time. (See the Dead Code section of this document for further discussion).

# Readability

There are 2 kinds of languages:
1. human readable, and,
2. machine readable.

The result needs to be machine-readable.

## Dead Code

Dead code ...
- only takes up space
- does not cost execution time
- is a problem for human readability, but is not a problem for machine readability (machines don't care).

_Dead Code_ is not really an issue, but can be removed automatically at a later time.  The dead code only takes up space but has very little impact on runtime performance, since the _bash_ interpreter skips the dead code.  Dead code is mostly an issue of human readability (whereas, this project concerns itself mostly with machine readability - see the Readability section).

# Future

- make this thing emit Python instead of BASH (I suspect that this is easy, hacking on `emit*.ohm` and `emit*.glue` should be enough)

- make this thing emit WASM (WAT)

- make this thing emit code for JVM and BEAM

- feed this thing to itself ("self compile", "eat your own dogfood")

- [_done: node-ify the whole thing, so that it all runs from the command line_]

- add hierarchical nodes, allow sub-sequences/details

- strip out unneeded code from support.js.


# Appendix

[Bare Essence](https://guitarvydas.github.io/2021/08/16/Bare-Essence.html).

[WASM Arithmetic Transpiler](https://guitarvydas.github.io/2021/05/15/WASM-Arithmetic-Transpiler.html)

[Arithmetic Example in Python/JS/etc](https://guitarvydas.github.io/2021/05/11/Ohm-Arithmetic.html)

[js-prolog](https://github.com/guitarvydas/js-prolog)

[Blog](https://guitarvydas.github.io)
[Videos](https://www.youtube.com/channel/UC2bdO9l84VWGlRdeNy5)
[References](https://guitarvydas.github.io/2021/01/14/References.html)

<script src="https://utteranc.es/client.js" 
        repo="guitarvydas/guitarvydas.github.io" 
        issue-term="pathname" 
        theme="github-light" 
        crossorigin="anonymous" 
        async> 
</script> 
