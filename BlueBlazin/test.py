from pprint import pprint
from lexer import Lexer
from parser import Parser
from interpreter import Interpreter
import json

if __name__ == "__main__":
    source = r"""
        let n = 1;

        function foo(m) {
            return m + n;
        }

        print foo(/* [1] enter some number here and it will be incremented */);

        /* [[1]] {{ 5 }} */
    """

    # pprint(list(Lexer(source)), indent=2)
    # pprint(Parser(source).parse(), indent=2)

    ast = Parser(source).parse()
    Interpreter(ast).run()
