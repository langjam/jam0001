# Tokenise a file.
# Owes a debt of gratitude to the scanner in https://craftinginterpreters.com/scanning.html.

from enum import Enum, auto


class Token(Enum):
    and_call_it = auto()
    binop = auto()
    bof = auto()
    code = auto()
    comparison = auto()
    done = auto()
    ditto = auto()
    dot = auto()
    eof = auto()
    eol = auto()
    function = auto()
    function_name = auto()
    header_end = auto()
    hfill = auto()
    identifier = auto()
    if_keyword = auto()
    negative = auto()
    number = auto()
    param = auto()
    returnvar = auto()
    setvar = auto()
    then = auto()
    to = auto()


class Tokeniser:
    def __init__(self, text):
        self.text = text
        self.start = 0
        self.current = 0
        self.token = None
        self.line = 1

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

    def advance_if(self, *args):
        # Sort args into reversed order of length for greedy matching.
        # print(f"advance_if '{self.text[self.current:self.current+10]}' args={args}")
        sorted_args = sorted(args, key=len, reverse=True)
        for s in sorted_args:
            peeked = self.text[self.start] + self.peek(len(s) - 1)
            if peeked.lower() == s.lower():
                self.current += len(s) - 1
                return True
        return False

    def function(self):
        while self.peek() == "*":
            self.advance()
        return Token.function

    def function_name(self):
        # A function name is terminated by a full stop and cannot go onto the next line.
        while self.peek() not in (".", "\n", ""):
            self.advance()
        if not self.done() and self.text[self.current] == ".":
            return Token.function_name

    def ignore(self):
        while self.peek() != "\n":
            self.advance()

    def skip_spaces(self):
        while self.peek() in " \t":
            self.advance()

    def identifier(self):
        p = self.peek()
        while p.isalnum() or p == "'":
            self.advance()
            p = self.peek()
        return Token.identifier

    def number(self):
        while self.peek().isdigit():
            self.advance()
        return Token.number

    def scan_token(self):
        ch = self.advance()
        if ch in "\n":
            self.line += 1
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

            if ch == " ":
                self.skip_spaces()

            # Is it the start of a code line?
            if ch == "/" and self.peek() == "/":
                self.advance()
                return Token.code

            # Is it an ignored line?
            if ch.isalpha():
                self.ignore()

        elif self.last == Token.hfill:
            if ch == " ":
                self.skip_spaces()

            if self.advance_if("@param"):
                return Token.param

            if self.advance_if("@return"):
                return Token.returnvar

            # Is it a function name?
            if ch.isalpha():
                return self.function_name()
        else:
            if self.advance_if("and we're done"):
                return Token.done

            if self.advance_if("If"):
                return Token.if_keyword

            if self.advance_if("then"):
                return Token.then

            if self.advance_if("set"):
                return Token.setvar

            if self.advance_if("to"):
                return Token.to

            if self.advance_if("ditto"):
                return Token.ditto

            if self.advance_if("and call it"):
                return Token.and_call_it

            if ch == ".":
                return Token.dot

            # Is it a positive number?
            if ch.isdigit():
                return self.number()

            # Is it a negative number?
            if ch == "-" and self.peek().isdigit():
                return Token.negative

            # Is it a binary operator on numbers?
            is_operator = ch in "+-/*%"
            is_spaced = self.peek().isspace()
            if is_operator and is_spaced:
                return Token.binop

            # Is it a boolean comparison?
            if self.advance_if(
                "is",
                "is less than",
                "is greater than",
                "is less than or equal to",
                "is greater than or equal to",
            ):
                return Token.comparison

            # Is it an identifier? This MUST go at the end because of how vague it is.
            if ch.isalpha():
                return self.identifier()

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

    #     text = """\
    #  * @param n
    # """
    tokeniser = Tokeniser(text)
    for token in tokeniser:
        print(token)
