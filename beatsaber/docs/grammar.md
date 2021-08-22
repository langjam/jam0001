# beat saber Grammar

The grammar of beat saber is made up of keywords, identifiers, and operators.

## Keywords

The following is a non-exhaustive list of all currently supported keywords in beat saber (and are reserved as such) and their usage:

- `is`
  - Used in assignment statements to bind an identifier to the result of an expression.
  - `// test is "hi"`
- `then`
  - Used to chain multiple operators together in expressions.
  - `a.. // yeet is inc then inc`
- `with`
  - Used to declare a function with one or two parameters. Functions can shadow values outside of functions, and can also use identifiers defined before. Identifiers are captured by value.
  - `// my_func is with a`
- `if`
  - Used to evaluate conditionally based off of an evaluated identifier.
  - `// true is 1`
  - `a.b // if true a is add`
- `goto`
  - Used to goto the evaluated result as a line number.
  - `// a is 1`
  - `a // goto is`
- `yeet`
  - Discard identifier. Specifies the result to be discared instead of saved.
  - `a.b // yeet is add`
- `not here`
  - Specifies a function as external.
  - `// printf is not here`
- `but is in`
  - Specifies a module name that an external function can be found in.
  - `// malloc_special is not here but is in libthing.so`
- `this is big`
  - Specifies an external function requires two parameters instead of one.
  - `// calloc is not here this is big`
- `return`
  - Specifies the return from a function as the evaluated expression.
  - `a.b // return is add`
- `still in`
  - Specifies that the provided line is still within the function with the provided identifier.
  - `// my_func is with a`
  - `a. // still in my_func return is inc`
- `and`
  - Specifies that a function has two parameters
  - `a.b // my_func is with a and b add`

## Identifiers

Identifiers are any sequence of alphanumeric characters that do not start with a number and may contain `_`.

Identifiers can be shadowed, except for functions, which must not be shadowed, including external functions.

Identifiers are typically used with operators, but are also used in behaviours. For example, `if`, `with`, `and`, and actual operations must be used with identifiers.

There are two special identifiers that exist implicitly: `argc` and `argv`.

## Operations

The only operator in beat saber is `.`

This is used for unary operations (`a.`) and binary operations (`a.b`).

In order to specify the operation, you must provide an operation for each `.` you provide in your statement. These will be bound to the operations in tree-parse order (first evaluated first).
