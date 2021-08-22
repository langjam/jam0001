# no-one-understands-my-vine-references-so-i-made-this-programming-language

If the name of this programming language doesn't make it clear, this is an
[esolang](https://en.wikipedia.org/wiki/Esoteric_programming_language). It
translates vine quotes into commands that run in a VM.

## Building and Running
Currently only *nix platforms are supported.

```
cd no-one-understands-my-vine-references-so-i-made-this-programming-language
cargo build

./target/debug/no-one-understands-my-vine-references-so-i-made-this-programming-language example.vine
```

## VM
The Virtual Machine for this language is extremely simple: two 64-bit
registers (`primary` and `secondary`), a stack, and a status register.

The language does provide a higher level for if / elseif / else / endif
statements that you may not expect. You may also define exactly one
function (however this feature is currently only partially implemented).

### Opcodes
The opcodes are listed below, and are quite simple. Each opcode does
exactly one thing; there are no parameters. Additionally, some of the
opcodes are not yet implemented. The opcodes are case and punctuation
sensitive.

Additional input between or surrounding the opcodes are ignored.

| Opcode | Description | Implemented? |
|:------ |:----------- | ------------ |
| Hi, welcome to Chili's | main function | yes |
| I never went to Oovoo Javer | `primary = 0` | yes |
| Eh | `primary = 1` | yes |
| Miss Keisha! Miss Keisha! | `primary = 0` | yes |
| Hey Ron | `primary = primary + 1` | yes |
| Hey Billy | `primary = primary - 1` | yes |
| Chris! Is that a weed?? | `primary = 911` | yes |
| Oooh | `primary = 101` | yes |
| The volume in this bus is astronomical | `primary = pow(primary, secondary)` | yes |
| Double cheeked up on a Thursday afternoon, | `primary = primary * 2` | yes |
| Look at all those chickens! | `primary = primary + secondary` | yes |
| It is Wednesday my dudes | `primary = primary ^ secondary` (XOR) | yes |
| Road work ahead? | Loop start | yes |
| Yeah I sure hope it does | Loop end | yes |
| Let's go to the beach beach! | push `primary` onto stack | yes |
| I can't swim | pop from stack into `primary` | yes |
| I am confusion | compare `primary` and `secondary` and set status register | yes |
| Happy Chrismis | `if status is EqualTo` | yes |
| Its Chrismin | `elseif status is LessThan` | yes |
| Merry Chrisis | `elseif status is GreaterThan` | yes |
| Merry Chrysler | `endif` | yes |
| And they were roomates | write byte in `primary` to file descriptor in `secondary` | yes |
| ELLO SUSIE | read byte into `primary` form file descriptor in `secondary` | yes |
| IMMA BUY ME A SUBARU | call function | no |
| I don't even know which way the Quiznos is, | `break` from loop | yes |
| I can't believe you've done this | open port number in `primary` | no |
| A child | close port number in `primary` | no |
| No off-topic questions | `panic` | yes |
| You have been stopped | `panic` | yes |
| Zack stop | `return` from function | yes |
| What are those?? | push `primary` onto stack, push `secondary` onto stack | yes |
| I'm a giraffe! | pop from stack into `secondary`, pop from stack into `primary` | yes |
| Wow | swap `primary` and `secondary` | yes |
| I'm in me mum's car | begin function definition | partial |
| Get out me car | end function definition | partial |

## LangJam Metadata
This language was created for LangJam0001. The prompt was "first class
comments" and so I interpreted that as "funny quotes."
