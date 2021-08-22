from pprint import pprint
from lexer import Lexer
from parser import Parser
import json

if __name__ == "__main__":
    source = r"""
        x = /* [1] left hand side */;
        y = /* [2] right hand side */;
        z = x + y;

        /* [[1]] {{5}} */
        /* [[2]] {{7}} */
    """

    # pprint(list(Lexer(source)), indent=2)
    pprint(Parser(source).parse(), indent=2)
