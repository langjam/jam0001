#!/usr/bin/env python3

import lark
import argparse
import readline

from parser import parser
from jlast import LiteralTransformer, ToAst, AstPrinter
from interpreter import Interpreter
from resolver import Resolver
from exceptions import JlException, format_backtrace
from jltypes import JlUnit, JlComment


def eval_source(filename, interpreter, source, debug=True, full_source=None, line_offset=0):
    if full_source is None:
        full_source = source
        
    try:
        parse_tree = parser.parse(source)
        if debug:
            print("Parse Tree:")
            print(parse_tree.pretty())
            print()
    
        tree_with_literals = LiteralTransformer(filename, line_offset).transform(parse_tree)
        ast = ToAst(filename, line_offset).visit(tree_with_literals)
        Resolver(interpreter.environment).visit(ast)
        if debug:
            print("Abstract Syntax Tree:")
            AstPrinter().visit(ast)
            print()

        value = interpreter.visit(ast)

        if debug:
            print("Global Environment after Evaluation:")
            for k, v in interpreter.environment.bindings.items():
                print(f"{k} = {repr(v)}")
            print()

        return value

    except lark.exceptions.UnexpectedToken as e:
        print(f"{filename}:{e.line}:{e.column} syntax error: expected one of {e.expected}"),
        print(e.get_context(source))
    except lark.exceptions.UnexpectedCharacters as e:
        print(f"{filename}:{e.line}:{e.column} syntax error: unexpected characters"),
        print(e.get_context(source))
    except lark.exceptions.UnexpectedEOF as e:
        print(f"{filename}:{e.line}:{e.column} syntax error: unexpected end of file"),
        print(e.get_context(source))
    except JlException as e:
        print(e.get_backtrace(full_source))
    except RecursionError:
        print(format_backtrace(interpreter.backtrace, full_source))
        print("maximum recursion depth exceded")

    return JlUnit(JlComment(":("))


def run_file(path, debug=False):
    with open(path) as f:
        source = f.read()

    i = Interpreter()
    value = eval_source(path, i, source, debug=debug)
    if debug:
        print("Program Return Value:")
        print(value)


def repl(debug=True, quiet=False):
    inter = Interpreter()
    full_source = ""
    line_offset = 0
    while True:
        try:
            source = input('>>> ')
        except KeyboardInterrupt:
            print()
            continue
        except EOFError:
            print()
            break
        full_source += source + '\n'
        value = eval_source("<repl>", inter, source, debug, full_source, line_offset)
        if not quiet:
            print("->", value, value.get_comment())

        inter.clear_backtrace()
        line_offset += 1


if __name__ == "__main__":
    argp = argparse.ArgumentParser(
        description="A programming language  with first-class Comments")
    argp.add_argument("file", type=str, nargs='?',
                      help="PlsExplain program to run")
    argp.add_argument("-d", "--debug", default=False, action="store_true",
                      help="print verbose debug info")
    argp.add_argument("-q", "--quiet", default=False, action="store_true",
                      help="don't print result of expressions when in interactive mode")
    args = argp.parse_args()
    if args.file is not None:
        run_file(args.file, args.debug)
    else:
        repl(args.debug, args.quiet)
