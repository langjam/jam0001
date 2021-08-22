# Lilith

> The theme of the jam is "first-class comments".

Okay so _enormous bong rip_ what is a comment anyway.

### Comments

```c
// foo bar baz
```

``` clojure
; text that the parser throws away

(def <name> <value>)
(def ^{:doc "foo bar baz"} <name> <value>)
```

Traditionally, comments are just text that the language throws away or that the compiler ignores.
For instance `//` or `;` comments just discard an entire line.
`/* ... */` or `(* ... *)` comments discard a block.

Languages that want to play games with comment inspection need customized parsers different from the one used by the language itself to extract this text which implementers usually just drop on the floor.

### Docstrings

Another take on the idea of putting text in the source file but privileging it more is docstrings - special strings understood by the language to have interpretation.

``` python
"""modules have __doc__ as a property defined by the FIRST heredoc-string in the file"""

def foo(a, b):
    """Adds a and b.

    >>> foo(1, 2)
    3
    """

    return a + b

```

``` clojure
(defn foo [a b]
    "Adds a and b

    >>> (foo 1 2)
    => 3
    "
    (+ a b))
```

But these are both deeply syntactically limited.
DSLs inside of strings .... are inside of strings.
Special separate parsers are required to work with or on them.
Users have to write those DSLs within the context of embedded strings.
It just doesn't work well.

What if we wanted to put other media in a "source" file?
What if the "doctest" grammar was something you could just type at the top level?
Or you could embed YAML or SQL into a file without special escaping airs?

## Enter Lilith

Lilith is a sketch at what if you took the ideas from literate programming (having fragments of text from which programs are composed) but deliberately DID NOT privilege the "source" for a "document" over the "source" for the "program".
Documents and programs could be co-equal and co-resident artifacts.

To achieve this vision, Lilith uses a context sensitive block-prefixed syntax which SHOULD be uncommon enough not to involve collisions with other languages.

Lilith is an [M-expression](https://en.wikipedia.org/wiki/M-expression) esque language with a "meta" language and two "object" languages.

The meta-language is `!` prefixed M-expressions.
At present the meta-language has two directives, `!def[<name>, <language>]` and `!import[<from>, ...]`.
This Lilith implementation is bootstrapped off of Python, and provides two built-in languages, `lil` AKA Lilith and `py` AKA python3.

Lilith interpretation is actually dual (or potentially N) interpreter based.
When a given name is evaluated, its body or definition is evaluated in the given language.
For instance, this snippet would define a pair of Lilith "foreign" functions in Python (`gt` and `sub`), which would then be used from the definiton of `fib`.

``` lilith
!def[gt, py]
return lambda x, y: x > y

!def[sub, py]
return lambda x, y: x - y

!def[fib, lil]
lambda[[x]
       , cond[[gt[x, 1],
                add[fib[sub[x, 1]], fib[sub[x, 2]]]],
              [true,
                0]]]
]
```

Where this gets really fun is that there are no restrictions on the number of sub-languages which Lilith can support.
For instance, Markdown could be a sub-language.

``` lilith
!def[docstring, md]
This module has a docstring, defined to be whatever the `md` language processor produces.
For instance, this could be compiled HTML.
Or it could be source markdown, post-validation.

When executed, this module will evaluate the docstring and print it.

!def[main, lil]
print[docstring]
```

We could even get real weird with it, embedding GraphViz graphs or YAML documents within a Lilith file and composing them all together.

### The Demo

``` shell
$ python3 setup.py develop
$ lil
>>> print["hello, world!"]
hello, world!
```

### Generating HTML with Markdown from the designdoc

``` shell
$ lil src/lark/designdoc.lil
<h1>The Lilith Pitch</h1>
<p>Code is more than .. just code for the compiler.
....
```

## Hackweek disclaimers

Python packaging is the devil.
This code was originally developed in [my monorepo](https://github.com/arrdem/source/tree/trunk/projects/lilith) where it is build and tested via a reproducible Bazel setup.
I don't want to try and make the judges install Bazel, so `python3 setup.py develop` is the lowest common denominator.
Note that `setup.py install` doesn't work for resource packaging reasons I haven't sorted out.

While this README may be stale of language features, at this time Lilith is mostly concerned with parsing, building a runtime/namespace system of and evaluating fragments.
While the machinery is there in the form of a relatively classical lisp `eval[]` operation to implement `if`, `lambda` and other such traditional special forms, that really wasn't the intent of the language syntax.
Most of Lilith's standard library (`src/python/lilith/prelude.lil`) consists of establishing a FFI prelude to Python, which makes the language seem more fully featured than it is.

**Currently missing:**
- `eval[]`
- `apply[]` (although this is trivial to implement)
- `if[]`
- `not[]`
- `=[]`
- `let[]`

## License

This code is copyright Reid D. 'arrdem' McKenzie 2021, published under the terms of the MIT license.
