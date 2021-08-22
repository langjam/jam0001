from pprint import pprint
from lexer import Lexer

if __name__ == "__main__":
    source = r"""
        x = /* [1] left hand side */
        y = /* [2] right hand side */
        z = x + y

        /* [[1]] {{5}} */
        /* [[2]] {{7}} */
    """

    lex = Lexer(source)
    pprint(list(lex.lex()), indent=2)
