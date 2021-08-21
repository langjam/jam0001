---
layout: post
title:  "Jam0001"
---
# Synopsis

Transpile drawings to code using drawio, PROLOG, Ohm-JS.

Components on the drawings are concurrent by default.

Four transpilers (languages) are created:
1. sequencing diagram -> JSON
2. details diagram -> JSON
3. sequence.json -> BASH
4. details.json -> BASH

# Introduction

The following is a discussion about the [language jam](https://github.com/langjam/langjam).

[_I find that writing about a problem helps me think about the problem. This may read as a stream of consciousness._]

# Build
# Discussion
To me, 1st class comments means that:
- if you change the comments, the execution changes.

This means that comments need to compile to code.

In my opinion, this means a separation between:
- DI (Design Intent), sometimes labelled as Software Architecture
- Implementation.

DI must result in Implementation.

On the first cut, I want DI to generate Implementation.

If Implementation is manually tweaked, then there should be some "providence" that shows how the DI was transmogrified into the Implementation code.

Why would anyone further tweak Implementation? 
- Optimization
- Testability
- Security
- etc., etc.

Is tweaking the same as "Round Trip"?
- No, Round Trip is hard.
- No, Round Trip usually means that anyone can tweak the Implementation to their hearts' content. In Structural Engineering, this does not happen, it's one-way only. The Implementors (Construction Workers) don't get to change the blueprints. It's one-way only. If a design change is required, the Implementor goes back to the Engineer and, if necessary, the Engineer goes back to the Architect. The blueprint gets modified by the signatory (usually an Engineer) and the Implementors must build according to the blueprint specification. DI'ers (Architects and Engineers) get paid big bucks, but are liable - in Law - for their designs. [Certified Engineers can be sued if their design fails.  It is _illegal_ to call yourself an Engineer unless you've been certified.].

Note that git, github, diff, etc. 

I see at least two ways to make languages for DI:
1. Cast the code as markdown.  Generate code from the .md file(s).
2. Cast the code as SVG diagrams, generate code from the diagrams.

I've done both of the above.

I'm going to start simple and work my way up.

# Bare Essentials
What are the _bare essentials_ for making DI languages for human use?
- concurrency
- isolation.

## Concurrency
Concurrency is necessary, because that is what normal humans are used to.  

For example, we teach 5 year-olds hard realtime and we use a several-hundred year old notation for hard realtime. Piano lessons.

For another example, when we cook dinner, we follow a concurrent specification, called a _recipe_ (while the potatoes cook, chop blah blah blah).

Normal concurrent solutions often fail when we try to scale them up. Spreadsheets work up-to-a-point, then collapse into a big mess. We don't need to switch to thinking synchronously to solve the scaling-up problem, but most programmers are taught this way of doing things and most programming languages are synchronous at their core (thread libraries are a symptom of anti-concurrency).

Programmers have concurrency beat out of them in university. A lot of accidental complexity has been caused by trying to use synchronous languages for asynchronous problems.

## Isolation
Build and forget.

Tweaking a component must not not affect how other components work. 

Developing a new component must not not affect how existing components work. 

Eschew dependency.

Unix does this, but this kind of isolation has been overlooked. Even a C program is isolated under Unix. 
- It doesn't matter if the C program has a garbage collector or not ; when the process dies, Unix cleans up after it.
- It doesn't matter if the program manipulates State or not. Programs are black boxes, we don't know (don't care) how they are implemented.  If a program runs too slow, give it to a Production Engineer. If a program is hard to maintain, give it to a Maintenance Engineer.

## Business Org

Businesses are arranged in a hierarchy with no sideways dependencies.

Each business unit is isolated from other business units.

Managers of business units report to their managers. 

VPs might oversee some 100's of workers, but the lines of communication are restricted. Every VP communicates (downwards) to a small number of department managers. Every VP reports to the CEO (upwards communication).  The CEO does not need to micro-manage each department because the departments are isolated and the department managers look after the details.

There is even a term, in business, for when workers break the organizational hierarchy. It's called "Going over your boss's head".

### Efficiency
In DI, efficiency doesn't matter much. The only thing that matters is if the DI design can be run "in a reasonable manner" by the DI Architect (and Engineer).

# Staring Small
## How?
Cheat as much as possible.

# Actual Problem
At this very moment, I am working on something that uses a _bash_ script to build.

The _bash_ script contains a lot of detail.

If I were tasked with maintaining this project, the first thing that I would want to know is "what is the bare essence of what this does?".

I don't trust text comments, so, as a maintainer, I would skip over the text comments and look at the code.

The code contains a lot of details and I need to build up a model in my head of what is going on.  The field of _design recovery_ researches this kind of issue.

At a very simple level, the first thing I would do is draw an ad-hoc diagram of the major steps in this script. I would draw it on a whiteboard, or, if working alone, I would draw it on a piece of paper.

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

## Desired Script

What I really want is something like...

![2021-08-20-jam0001-top level.png](https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/2021-08-20-jam0001-top%20level.png?raw=true)

## Making It Simpler

I have less than 24 hours to build this.

What can I chop to make this simpler?

The boxes represent at least 2 views of the solution:

1. sequencing of operations
2. details.

Let's start by making separate diagrams for each of the view.

## Sequence Diagram

![2021-08-20-jam0001-sequence.png](https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/2021-08-20-jam0001-sequence.png?raw=true)

## Details Diagram

![2021-08-20-jam0001-details.png](https://github.com/guitarvydas/jam0001/blob/guitarvydas/guitarvydas/2021-08-20-jam0001-details.png?raw=true)

"Red" boxes mean _synchronous_ code.  (The default is asynchronous code).



# Appendix

[Bare Essence](https://guitarvydas.github.io/2021/08/16/Bare-Essence.html).

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
