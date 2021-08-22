# COMP

The COMment Processor language written for https://github.com/langjam/jam0001

This language celebrates the multitude of comment syntaxes in programming languages and uses them to encode a simple stack-based interpreter.

## Basic Usage

This project requires a recent version of node/npm to be installed (developed on 7.20.3), see https://nodejs.org/en/download/

After you first check out this code make sure you run:

`npm install`

 * To run tests issue the following command:

`npm t`

 * To run a script issue the following command, replacing the filename accordingly:

`npx ts-node ./lib/comp.ts ./test/resources/singleLine/REM.COMP`

 * To run the REPL issue the following command

`npx ts-node ./lib/comp.ts`

Press Ctrl+C to exit.

## Example

```
REM 1 this is a comment that takes a single argument and put it on the stack
REM 2 arguments are always the first part of a comment
REM 3 the stack is now [1,2,3]
-- this comment takes no arguments and pops a value from the stack, which is now [1,2]
; this should roll so the stack is now [2,1]

# add this command executes a procedure so now the stack is [3]

REM 4
# mul should result in [12]
```

See `test/resources/` for more examples.

## Single Line Comments

| Comment | Origin   | Function  | Args      |
| ------- | -------- | --------- | --------- |
| `REM`   | BASIC    | push      | value     |
| `--`    | Haskell  | pop       | -         |
| `#`     | Perl     | call      | proc name |
| `//`    | C-style  | duplicate | -         |
| `;`     | Assembly | roll      | -         |
| `%`     | MATLAB   | swap      | -         |

## Values

Currently only integers are supported as input although regular JavaScript numbers are used on the stack.

## Procedures

When you issue a `#` comment you can follow that comment with `add`, `sub`, `mul`, or `div` to perform the given procedure on the top two values.

## Future Plans

I didn't get to do everything I originally planned to do with project.
Below are some improvements that could be made.

### Support More Types

It would be nice to support something other than numbers.
Ideally all basic JavaScript types could be supported.

### Error Handling

There is none.

### User Defined Procedures

I was originally planning on using multi-line comments (like `/* */` or `<!-- -->`) to allow users to define their own procedures but I never got around to it.

### Conditionals

Similar to the above I was planning on supporting basic conditionals with multi-line comments as well.
