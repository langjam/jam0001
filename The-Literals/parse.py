import tokenise


class Parser:
    def __init__(self, tokeniser):
        self.tokeniser = tokeniser

    def parse(self):
        pass


if __name__ == "__main__":
    input_file = "samples/fib.comment"
    with open(input_file, "r") as f:
        text = f.read()

    tokeniser = tokenise.Tokeniser(text)
    parser = Parser(tokeniser)
    parser.parse()
