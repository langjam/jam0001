from tokenise import Tokeniser
from parse import Parser


def run(input_file):
    try:
        with open(input_file, "r") as f:
            text = f.read()
    except IOError:
        print(f"Failed to read file '{input_file}'", file=sys.stderr)
        return -1

    tokeniser = Tokeniser(text)
    parser = Parser(tokeniser.tokenise().__next__)
    program = parser.parse()
    program.execute()


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Please supply a filename.", file=sys.stderr)
        sys.exit(-2)

    sys.exit(run(sys.argv[1]))
