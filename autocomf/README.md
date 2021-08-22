# AUTOCOMF - Automatic Configuration from Comments

**AUTOCOMF** is a programming language that presents a configuration
UI based on annotations in a source file.  The configuration specified
by the user is then incorporated by automatically modifying the source
file.

**AUTOCOMF** can be used seamlessly with *any* programming language or
configuration language, since it is written wholly within the comment
syntax of the host language.

**AUTOCOMF** has incorporates several attractive qualities:

* Self-modifying code.

* Semantically meaningful comments.

* ALL CAPS KEYWORDS.

* A human-friendly, intuitive, unclearly specified syntax.

* Named in honour of the universally loved
  [autoconf](https://www.gnu.org/software/autoconf/) system.

**AUTOCOMF** is guaranteed free of defects, but user error is always a
possibility.  In the best Unix tradition, **AUTOCONF** will
irrevocably change the input file, without any backup!  Make sure you
understand [the completely precise and unambiguous
specification](https://github.com/athas/autocomf/blob/main/src/Main.hs)
before running it on files you care about!

**AUTOCOMF** was developed as a contribution to [Langjam
#1](https://github.com/langjam/jam0001).

## How does AUTOCOMF work?

AUTOCOMF code is embedded inside comments of any other programming
language or config file, although currently limited to the ones that
support line comments (such as `#` for shell scripts, `;` for Lisp, or
`//` for C).  The input file must somewhere contain the string
`AUTOCOMF COMMENT LINE foo`, after which `foo` is taken to be the line
comment marker for the file.  This string is typically itself embedded
within a line comment.  For example:

```
# AUTOCOMF COMMENT LINE #
```

The rest of the examples will assume that `#` is the line comment
marker.

An AUTOCOMF program consists of *variable definitions* and
*directives*. When an AUTOCOMF program is run, the variable
definitions will be collected and presented to the user, who can then
interactively provide values for them. Here is an example of a
variable definition:

```
# AUTOCOMF VAR myForegroundColour REGEX red|green|blue|purple|cyan|white|yellow
```

This defines a variable called `myForegroundColour`, whose value must
match the regular expression provided.

After values have been provides for all the variables, the directives
are then interpreted.  Each directive controls the following
(presumably non-comment) line.  For example:

```
# AUTOCOMF USE fg=$¤myForegroundColour¤
fg=$red
```

The line `fg=$red` will be replacd by `fg=$foo`, where `foo` is the
value of the `myForegroundColour` specified by the user.  Note the use
of the `¤` character to bracket AUTOCOMF-level variables - because they
are
[language-unspecific](https://en.wikipedia.org/wiki/Currency_sign_(typography))!

After these substitutions are done, the source file will be
overwritten with the result.

## Examples

[They are here.](examples/)

## Compiling and using

You'll need a Haskell setup (install `ghc` and `cabal` from your
package manager [or use ghcup](https://www.haskell.org/ghcup/)).  Then
build with:

```
$ cabal update
$ cabal build
```

And run an example program with:

```
$ cabal run autocomf examples/bashrc
```
