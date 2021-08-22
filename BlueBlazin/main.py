import sys
from pathlib import Path
from interpreter import Interpreter
from parser import Parser


def main():
    if len(sys.argv) < 2:
        sys.stderr("Usage: main <filepath>")
        return

    source = Path(sys.argv[1]).read_text()
    ast = Parser(source).parse()
    Interpreter(ast).run()


if __name__ == "__main__":
    main()
