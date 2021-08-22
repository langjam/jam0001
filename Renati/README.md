# Renjamlang (RJL)

Renjamlang (aka RJL) is a scripting language made in 48h in C++ for the first
langjam hosted at [langjam/jam0001](https://github.com/langjam/jam0001). The
theme was *First-class comments*.

In this language, comments can be attached to values and have string
interpolation ability, so you can substitute parts of your comments with
variables defined in the scope of a comment.

Most of the syntax uses *Reverse RPN* (prefix notation), because ~~it's easier
to parse and there's no operator precedence~~ it's superior and doesn't require
parentheses. Comments are old C-style `/* This is a comment. */`, but can
reference variables. For example:

`/* The value of x is $x. */ = x 10`

This line sets the value of `x` to `10` and attaches a comment. When you later
type in `x` in REPL to print the value, you will get this output:

```
/* The value of x is 10. */
10
```

You get the comment with `$x` substituded for the actual value of `x` and the
value of `x` below. Go to the [bottom](#comments) if you want to know more about
comments.

Small example to try in REPL:

```
> = add fn (a b) /* Result of adding $a and $b. */ return + a b end
> add (1 3)
/* Result of adding 1 and 3. */
4
> add (4 2)
/* Result of adding 4 and 2. */
6
```

Fun fact: this language doesn't use semicolons *and* doesn't care about
whitespace (spaces and newlines are treated equally), so you can have a program
that is just a giant single line.

# Building

You need `g++`. Run `./build.sh` or this:

```
g++ -std=c++17 -pedantic -Wall -Wextra -g -o rjl Common.cpp Main.cpp Lexer.cpp Parser.cpp Interpreter.cpp
```

After building run `./rjl` to get a REPL or `./rjl FILE` to read and execute a
script in `FILE`.

# Examples

Examples are available at [Example](./Examples) directory or below.

## Closures

**Command:**

```
./rjl Examples/closures.rjl
```

**Script:**

```
= random fn ()
  /*
  Chosen by fair dice roll.
  Guaranteed to be random.
  */
  return 4
end

= make_adder fn (a)
  /* Function that adds $a to a number. */
  return fn (x)
    /* Result of adding $a and $x. */
    return + a x
  end
end

= adder1 make_adder (1)
= adder4 make_adder (4)

random ()

adder1
adder4

adder1 (10)
adder4 (20)
make_adder (100) (200)
```

**Output:**

```
/*
Chosen by fair dice roll.
Guaranteed to be random.
*/
4
/* Function that adds 1 to a number. */
fn (x)
/* Function that adds 4 to a number. */
fn (x)
/* Result of adding 1 and 10. */
11
/* Result of adding 4 and 20. */
24
/* Result of adding 100 and 200. */
300
```

## Prime test

**Command:**

```
./rjl Examples/prime.rjl
```

**Script:**

```
= isPrime fn (a)
  if <= a 1
    return false
  end
  = i 2
  while <= * i i a
    if == % a i 0
      /* $a is not prime (divisible by $i). */
      return false
    end
    = i + i 1
  end
  /* $a is prime. */
  return true
end

= i 2 while <= i 10
  isPrime (i)
  = i + i 1
end
```

**Output:**

```
/* 2 is prime. */
true
/* 3 is prime. */
true
/* 4 is not prime (divisible by 2). */
false
/* 5 is prime. */
true
/* 6 is not prime (divisible by 2). */
false
/* 7 is prime. */
true
/* 8 is not prime (divisible by 2). */
false
/* 9 is not prime (divisible by 3). */
false
/* 10 is not prime (divisible by 2). */
false
```

## Sieve of Eratosthenes

**Command:**

```
./rjl Examples/sieve.rjl
```

**Script:**

```
= sieve fn (n)
  = A []
  = i 0
  while <= i n
    push A 1
    = i + i 1
  end
  = i 2
  while <= * i i n
    if == @ A i 1
      = j * i i
      while <= j n
        = @ A j 0
        = j + j i
      end
    end
    = i + i 1
  end
  = out []
  = i 2
  while <= i n
    if == @ A i 1 push out i end
    = i + i 1
  end
  /* Prime numbers from 2 to $n. */
  return out
end

sieve (100)
```

**Output:**

```
/* Prime numbers from 2 to 100. */
[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]
```

# Details

Types:

* void
* bool (`false`, `true`)
* number (double-precision floating-point, e.g. `1`, `2`, `0.5`)
* array (array of numbers only, e.g. `[]`, `[0 1 2]`)
* function (first-class, e.g. `fn () end`, `fn () return 1 end`, `fn (a b) return - a b end`)

## Statements

A well-formed program is a sequence of statements.

**If**

```
if CONDITION
    STATEMENTS
end

if CONDITION
    STATEMENTS
else
    STATEMENTS
end

if CONDITION
    STATEMENTS
elif CONDITION
    STATEMENTS
else
    STATEMENTS
end
```

**While**

```
while CONDITION
    STATEMENTS
end
```

**Assignment**

```
= IDENTIFIER EXPRESSION
```

**Array write**

Write a value to an array. `IDENTIFIER` has to refer to existing array.
`INDEX` is an expresion of type number. `VALUE` is an expression of type number.
This statement cannot change array's length.

```
= @ IDENTIFIER INDEX VALUE
```

**Array push**

Increase length of an array by one by pushing a value. `IDENTIFIER` has to refer
to exisiting array. `VALUE` is an expression of type number.

```
push IDENTIFIER VALUE
```

**Array pop**

Decrease length of an array by one by removing last value. `IDENTIFIER` has to
refer to exising array. Note that this is not an expression and doesn't return
any value.

```
pop IDENTIFIER
```

**Return**

Returns any value from a function. Can't be used at top-level code.

```
return EXPRESSION
```

**Expression**

Any expression is also a valid statement. If statement is an expression and the
expression evaluates to a non-void value, then the value is printed (with its
comment, if any attached). If you don't want to print an expresion, use `void`
unary operator.

## Expressions

**Bool literal**

```
false
true
```

**Number literal**

Negative number literals are not supported. If you want a negative number, use
`neg` unary operator.

```
0
1
0.5
```

**Array literal**

Array literal consists of brackets `[]` with any number of expressions inside.
Expressions have to evaluate to a number.

```
[]
[0]
[0 1 2 3 4 5 6 7 8 9]
[+ 1 2 - 2 3 * 4 5 / 6 7]
```

**Function literal**

Function literal requires argument list, which consists of parentheses `()` with
any number of identifiers inside. Types of arguments and return value are not
specified (could be of any type at runtime). If no return statement is
encountered function returns void.

```
fn ()
    STATEMENTS
end

fn (a)
    STATEMENTS
end

fn (a b c d)
    STATEMENTS
end
```

**Unary operators**

* `not BOOL` – logical not
* `neg NUMBER` – negates a number
* `void EXPRESSION` – turns any expression into void value
* `# ARRAY` – gets length of an array

**Binary operators**

* `+ NUMBER NUMBER`
* `- NUMBER NUMBER`
* `* NUMBER NUMBER`
* `/ NUMBER NUMBER`
* `% NUMBER NUMBER`
* `and BOOL BOOL`
* `or BOOL BOOL`
* `xor BOOL BOOL`
* `< NUMBER NUMBER`
* `> NUMBER NUMBER`
* `<= NUMBER NUMBER`
* `>= NUMBER NUMBER`
* `== NUMBER NUMBER`
* `!= NUMBER NUMBER`
* `@ ARRAY INDEX` – reads a value from an array

**Identifier**

Identifier respects the regular expression `[_a-zA-Z][_a-zA-Z0-9]*` and is not
one of reserved keywords:

* `and`
* `elif`
* `else`
* `end`
* `false`
* `fn`
* `if`
* `neg`
* `not`
* `or`
* `pop`
* `push`
* `return`
* `true`
* `void`
* `while`
* `xor`

Identifier not assigned to a value in current scope will evaluate to void.

**Call**

`FUNCTION` is an expression that evaluates to a function. `ARGUMENTS` is any
number of expressions. When a function is called, the number of actual and
formal parameters has to be the same.

```
FUNCTION (ARGUMENTS)
```

## Comments

Comments start with `/*` and end with `*/`. Comments cannot nest. Inside a
comment you can refer to a variable by using `$IDENTIFIER` syntax, i.e. `$`
immediately followed by an identifier. If you want to get literal `$`, use `$$`.
Comments capture variables by name, not by values. That means that a comment
holds a reference to a scope in which it was defined. References to variables
inside a comment are evaluated and substituded when comment gets printed out.

Any value (not necessarily a variable) can have a comment attached to it. To do
this, simply type a comment before any expression (except call). For example:

```
/* This is the number one. */ 1
/* Empty array. */ []
/* Sum of some numbers. */ + 10 20
```

Note that while an array can have a comment attached, its elements cannot.

The comments can propagate through operations under certain conditions:

* All unary operators copy comment of its operand to its result (unless the
  operation is preceeded by a comment, which then takes precedence).
* All binary operators copy comment to its result if one and only one of its
  operands have a comment (unless the operation is preceeded by a comment, which
  then takes precedence).
* Identifier will evaluate to a value with its comment, but the comment can be
  overriden by preceeding the identifier with a comment.

Return and assignment statements can have a comment preceeding them, which will
get attached to returned or assigned value, respectively. Such comment will take
precedence over logic described above and override the comment that resulted
from expression evaluation (if any).

Other statements can have comments preceeding them, but they carry no semantics.
