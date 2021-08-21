# Comstruct

### The Constructor is named `comment` / Probably some other things too

## Content

1. [Compiling the Interpreter](#Compiling-the-Interpreter)

## Compiling the Interpreter

![](https://img.shields.io/badge/Tested-Arch%20Linux-green)

You can compile the interpreter into a single executable, but you don't have to. You can either execute
the `comstruct.py` file from the `src`-directory or the built binary in `dist/`.

On (pretty much) any sytem with python 3.9 and all the [requirements](requirements.txt) installed, in (pretty much) any
shell go to the Floeckchengrafik/src folder of the cloned repo and
run `pyinstaller comstruct.py application_stack_utils.py cli.py executor.py internals.py lexer.py parser.py -c -F -p . --distpath ../dist --collect-all sly`
.
