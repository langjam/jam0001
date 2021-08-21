from enum import Enum, auto


class Token(Enum):
    binop = auto()
    bof = auto()
    eof = auto()
    eol = auto()
    function = auto()
    hfill = auto()
    header_end = auto()
    identifier = auto()
    code = auto()
    number = auto()
    param = auto()
    returnvar = auto()


class Tokeniser:
    def __init__(self, text):
        self.text = text
        self.start = 0
        self.current = 0
        self.token = None

    def done(self):
        return self.current >= len(self.text)

    def advance(self):
        ch = self.text[self.current]
        self.current += 1
        return ch

    def peek(self, n=1):
        if self.done() or self.current + n > len(self.text):
            return ""
        peeked = self.text[self.current : self.current + n]
        return peeked

    def match(self, expected):
        if self.done():
            return False
        if self.text[self.current] != expected:
            return False
        self.current += 1
        return True

    def advance_if(self, *args):
        for s in args:
            peeked = self.peek(len(s))
            if peeked == s:
                self.current += len(s)
                return True
        return False

    def function(self):
        while self.peek() == "*":
            self.advance()
        return Token.function

    def ignore(self):
        while self.peek() != "\n":
            self.advance()

    def identifier(self):
        while self.peek().isalnum() or self.peek() == "'":
            self.advance()
        return Token.identifier

    def number(self):
        while self.peek().isdigit():
            self.advance()
        return Token.number

    # TODO:
    # - function name
    # - Negative numbers
    # - and call it
    # - and we're done
    # - if
    # - then
    # - set
    # - to
    # - dot
    def scan_token(self):
        ch = self.advance()
        if ch in "\n":
            return Token.eol

        # Contextual scanning for tokens that can appear at the start of a line.
        if self.last in (Token.bof, Token.eol):
            # Is it a function?
            if ch == "/" and self.peek(2) == "**":
                return self.function()

            # Is it the end of a function signature?
            if self.advance_if(" */", "*/"):
                return Token.header_end

            # Is it a header filler?
            if ch == " " and self.peek() == "*":
                self.advance()
                return Token.hfill

            # Is it the start of a code line?
            if self.advance_if("//", " //"):
                return Token.code

            # Is it an ignored line?
            if ch.isalpha():
                self.ignore()
        else:
            if self.advance_if("@param"):
                return Token.param

            if self.advance_if("@return"):
                return Token.returnvar

            # Is it an identifier?
            if ch.isalpha():
                return self.identifier()

            # Is it a number?
            if ch.isdigit():
                return self.number()
            
            # Is it a binary operator?
            if ch == '+' or ch == '-' or ch == '/' or ch == '*' or ch == '%':
                return Token.binop

        return None

    def tokenise(self):
        self.start = 0
        self.current = 0

        token = Token.bof
        yield (token, "")
        self.last = token

        while not self.done():
            self.start = self.current
            token = self.scan_token()
            if token is not None:
                yield (token, self.text[self.start : self.current])
                self.last = token

        yield (Token.eof, "")
        self.last = token

    def __iter__(self):
        return self.tokenise()


if __name__ == "__main__":
    input_file = "samples/fib.comment"
    with open(input_file, "r") as f:
        text = f.read()

    print(text)

    tokeniser = Tokeniser(text)
    for token in tokeniser:
        print(token)
