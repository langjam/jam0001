#!/bin/env python3

from sys import argv, path
import os

import executor

path.append(os.path.dirname(__file__))

from lexer import ComstructLexer, FuncDescProcessor
from parser import ComstructParser
from executor import ComstructExecutor

if __name__ == '__main__':
    clexer = ComstructLexer()
    cparser = ComstructParser()

    if len(argv) != 2:
        while True:
            text = input("comstruct » ")
            tokens = clexer.tokenize(text)
            tree = cparser.parse(tokens)

            try:
                ComstructExecutor(tree)
            except Exception as e:
                print(e)

            environment = executor.env

    file = open(argv[1]).read()

    # print(" |------------|------------|------------|")
    # print(" |    Type    |    Value   |    Line    |")
    #
    # for token in clexer.tokenize(file):
    #     print(" |------------|------------|------------|")
    #     print(" | {:10} | {:10} | {:10} |".format(token.type, str(token.value), token.lineno))
    #
    # print(" |------------|------------|------------|")
    # print()

    tokens = clexer.tokenize(file)
    tree = cparser.parse(tokens)

    # print(" » Begin Execution")
    ComstructExecutor(tree)

    environment = executor.env
    print("")
