# Tokenise a file.
# Owes a debt of gratitude to the scanner in https://craftinginterpreters.com/scanning.html.

from enum import Enum, auto

from comparator_constants import *


class Token(Enum):
    AND = auto()
    AND_CALL_IT = auto()
    AS = auto()
    BINOP = auto()
    BACK = auto()
    BOF = auto()
    CODE = auto()
    COMPARISON = auto()
    DITTO = auto()
    DOT = auto()
    END_DEF = auto()
    EOF = auto()
    EOL = auto()
    FORWARD = auto()
    FUNCTION = auto()
    FUNCTION_NAME = auto()
    HEADER_END = auto()
    HFILL = auto()
    IDENTIFIER_WORD = auto()
    IF_KEYWORD = auto()
    JUMP = auto()
    LEAVE_FUNC = auto()
    LINES = auto()
    NUMBER = auto()
    NEGATIVE = auto()
    PARAM = auto()
    RETURNVAR = auto()
    SETVAR = auto()
    STRING_LITERAL = auto()
    THEN = auto()
    TO = auto()
    WITH = auto()


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

    def advance_if(self, *args, case_sensitive=False):
        # Sort args into reversed order of length for greedy matching.
        # print(f"advance_if '{self.text[self.current:self.current+10]}' args={args}")
        sorted_args = sorted(args, key=len, reverse=True)
        for s in sorted_args:
            peeked = self.text[self.start] + self.peek(len(s) - 1)
            if not case_sensitive:
                peeked = peeked.lower()
                s = s.lower()
            if peeked == s:
                self.current += len(s) - 1
                return True
        return False

    def function(self):
        while self.peek() == "*":
            self.advance()
        return Token.FUNCTION

    def function_name(self):
        # A function name is terminated by a full stop and cannot go onto the next line.
        while self.peek() not in (".", "\n", ""):
            self.advance()
        if not self.done() and self.text[self.current] == ".":
            return Token.FUNCTION_NAME

    def string_literal(self):
        # A string literal surrounded by double quotes.
        while self.peek() != "\"":
            self.advance()
        self.advance()
        if not self.done():
            return Token.STRING_LITERAL

    def ignore(self):
        while self.peek() != "\n":
            self.advance()

    def skip_spaces(self):
        while self.peek() in " \t":
            self.advance()

    def identifier_word(self):
        p = self.peek()
        while p.isalnum() or p == "'":
            self.advance()
            p = self.peek()
        return Token.IDENTIFIER_WORD

    def number(self):
        while self.peek().isdigit():
            self.advance()
        return Token.NUMBER

    def scan_token(self):
        ch = self.advance()
        if ch in "\n":
            self.line += 1
            return Token.EOL

        # Contextual scanning for tokens that can appear at the start of a line.
        if self.last in (Token.BOF, Token.EOL):
            # Is it a function?
            if ch == "/" and self.peek(2) == "**":
                return self.function()

            # Is it the end of a function signature?
            if self.advance_if(" */", "*/"):
                return Token.HEADER_END

            # Is it a header filler?
            if ch == " " and self.peek() == "*":
                self.advance()
                return Token.HFILL

            if ch == " ":
                self.skip_spaces()

            # Is it the start of a code line?
            if ch == "/" and self.peek() == "/":
                self.advance()
                return Token.CODE

            # Is it an ignored line?
            if ch.isalpha():
                self.ignore()

        elif self.last == Token.HFILL:
            if ch == " ":
                self.skip_spaces()

            if self.advance_if("@param"):
                return Token.PARAM

            if self.advance_if("@return"):
                return Token.RETURNVAR

            # Is it a function name?
            if ch.isalpha():
                return self.function_name()
        else:
            if self.advance_if("and we're done", case_sensitive=True):
                return Token.LEAVE_FUNC

            if self.advance_if("And we're done", case_sensitive=True):
                return Token.END_DEF

            if self.advance_if("If "):
                return Token.IF_KEYWORD

            if self.advance_if("then "):
                return Token.THEN

            if self.advance_if("set "):
                return Token.SETVAR

            if self.advance_if("to "):
                return Token.TO

            if self.advance_if("ditto"):
                return Token.DITTO

            if self.advance_if("and call it"):
                return Token.AND_CALL_IT

            if self.advance_if("and "):
                return Token.AND

            if self.advance_if("with "):
                return Token.WITH
            
            if self.advance_if("jump "):
                return Token.JUMP

            if self.advance_if("back "):
                return Token.BACK

            if self.advance_if("forward "):
                return Token.FORWARD

            if self.advance_if("lines"):
                return Token.LINES

            if self.advance_if("as "):
                return Token.AS

            if ch == ".":
                return Token.DOT

            # Is it a string literal?
            if ch == '"':
                return self.string_literal()

            # Is it a positive number?
            if ch.isdigit():
                return self.number()

            # Is it a negative number?
            if ch == "-" and self.peek().isdigit():
                return Token.NEGATIVE

            # Is it a binary operator on numbers?
            is_operator = ch in "+-/*%"
            is_spaced = self.peek().isspace()
            if is_operator and is_spaced:
                return Token.BINOP

            # Is it a boolean comparison?
            if self.advance_if(
                EQUAL,
                LESS_THAN,
                GREATER_THAN,
                LT_OR_EQ,
                GT_OR_EQ,
            ):
                return Token.COMPARISON

            # Is it an identifier? This MUST go at the end because of how vague it is.
            if ch.isalpha():
                return self.identifier_word()

        return None

    def tokenise(self):
        self.start = 0
        self.current = 0

        token = Token.BOF
        yield (token, "")
        self.last = token

        while not self.done():
            self.start = self.current
            token = self.scan_token()
            if token is not None:
                yield (token, self.text[self.start : self.current])
                self.last = token

        yield (Token.EOF, "")
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
