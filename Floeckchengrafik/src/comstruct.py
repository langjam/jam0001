#!/bin/env python3

from sys import argv, path
import os

path.append(os.path.dirname(__file__))

from lexer import ComstructLexer
from parser import ComstructParser
from executor import ComstructExecutor


if __name__ == '__main__':
    if len(argv) != 2:
        print("You need to specify exactly one file as an argument")
        exit(0)

    file = open(argv[1]).read()

    clexer = ComstructLexer()
    cparser = ComstructParser()

    print(" |------------|------------|------------|")
    print(" |    Type    |    Value   |    Line    |")

    for token in clexer.tokenize(file):
        print(" |------------|------------|------------|")
        print(" | {:10} | {:10} | {:10} |".format(token.type, str(token.value), token.lineno))

    print(" |------------|------------|------------|")
    print()

    tokens = clexer.tokenize(file)
    tree = cparser.parse(tokens)

    ComstructExecutor(tree)
