# The pseudocoder

JT's project (not part of the official jam, just for fun :) )

This project parses a simple stack-based expression language. To it, it adds comments as required values in the language.

The benefit of these comments is that in addition to the `run` subcommand which will evaluate the source, there's a `pseudo` subcommand that will create a pseudo-code source list based on the comments in your program.

Here's an example:

```
> jam pseudo examples/call.jam
function 'main':
  print hello world

function 'print':
  <internal print>

function 'foo':
  return hello
```
