from pprint import pprint
from lexer import Lexer
from parser import Parser
from interpreter import Interpreter
import json

if __name__ == "__main__":
    # source = r"""
    #     let n = 1;

    #     function foo(m) {
    #         return m + n;
    #     }

    #     print foo(/* [1] enter some number here and it will be incremented */);

    #     /* [[1]] {{ 5 }} */
    # """
    source = r"""
        let i = /* [1] initialize counter to 1 */;

        while (i < 100) {
            if (/* [2] i is divisible by both 3 and 5 */) {
                print "FizzBuzz";
            } else if (/* [3] i is divisible by 3 */) {
                print "Fizz";
            } else if (/* [4] i is divisible by 5 */) {
                print "Buzz";
            } else {
                print i;
            }
        }

        /* [[1]] {{0}} */
        /* [[2]] {{ i % 15 == 0 }} */
        /* [[3]] {{ i % 3 == 0 }} */
        /* [[4]] {{ i % 5 == 0 }} */
    """

    pprint(list(Lexer(source)), indent=2)
    # pprint(Parser(source).parse(), indent=2)

    # ast = Parser(source).parse()
    # Interpreter(ast).run()
