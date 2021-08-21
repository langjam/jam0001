#!/bin/env python3

import executor

from lexer import ComstructLexer
from parser import ComstructParser
from executor import ComstructExecutor


if __name__ == '__main__':

    clexer = ComstructLexer()
    cparser = ComstructParser()

    # print(" |------------|------------|------------|")
    # print(" |    Type    |    Value   |    Line    |")
    #
    # for token in clexer.tokenize(file):
    #     print(" |------------|------------|------------|")
    #     print(" | {:10} | {:10} | {:10} |".format(token.type, str(token.value), token.lineno))
    #
    # print(" |------------|------------|------------|")
    # print()

    while True:
        text = input("comstruct » ")
        tokens = clexer.tokenize(text)
        tree = cparser.parse(tokens)

        print(" » Begin Execution")
        ComstructExecutor(tree)

        environment = executor.env
        print(" » End Execution")
