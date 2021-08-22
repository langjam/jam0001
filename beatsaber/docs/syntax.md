# beat saber Syntax

## High Level

beat saber is made up of lines. Each line is typically a statement, except in the case of a blank or empty line, in which case it is nothing.

As an example, each of these lines is a statement.

```beatsaber
// puts is not here
// res is "beat saber"
res. // yeet is puts
```

All values in beat saber are 64bit unsigned integers. Pointers are expressed no differently than other numbers, as is the case for string literals.

## Statements

Each line (that is not a comment) is perceived as a statement.

Statements are made up of a behaviour (represented after a `//`) and an optional series of expressions.

All statements in beat saber can be likened to bindings or assignment.

From the hello world example:

```beatsaber
// puts is not here
// res is "beat saber"
res. // yeet is puts
```

Note that the first statement is an assignment statement to the identifier: `puts`, describing it as an external function that is `not here`.

The second statement is an assignment statement that assigns `res` the value of the string literal described by `"beat saber"`. More specifically, `res` is a pointer to the front of the array of characters that represent the string literal.

The third statement is an assignment statement that discards the value that would otherwise be assigned (`yeet`) with the result being a call expression that calls `puts` with a single parameter being `res`.

For clarity, the above would essentially boil down to the following C:

```c
char* res = "beat saber";
puts(res);
```

## Expressions

Identifiers, optionally paired with operators, are expressions.

They are evaluated at the point of execution with when they fire, in tree parsing order.

The following are some examples of expressions:
`a`, `a.`, `a.b`

The first is simply the "value of" identifier `a`, while the second and third are call expressions. That is, they perform a unary (`a.`) and binary operation (`a.b`) as determined in the behaviour.

## Behaviours

Behaviours are what make up the latter half of a statement. They must always be after the "keyword" `//`.

Behaviours include assignment, function declarations/definitions, among many other types of behaviour. Ultimately, they are what control the flow of a beat saber program and also determine the operations to perform on expressions.

See [grammar](grammar.md) for more details on behaviours and how they can be constructed from keywords.
