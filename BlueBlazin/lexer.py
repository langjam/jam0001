from enum import Enum


class TokenType(Enum):
    PUNCTUATOR = 0
    IDENTIFIER = 1
    KEYWORD = 2
    NUMBER = 3
    COMMENT = 4
    EOF = 5
    BOOLEAN = 6
    STRING = 7
    NULL = 8


class Lexer:
    def __init__(self, source):
        self.source = source.rstrip()
        self.idx = 0
        self.line = 0
        self.references = {}
        self.in_reference = False

    def __iter__(self):
        tokens = list(self._lex())

        for token in tokens:
            if token["type"] == TokenType.COMMENT:
                yield from self.references[token["value"]]
            else:
                yield token

    def _lex(self):
        while self.idx < len(self.source):
            self.consume_whitespace()

            match self.peek():
                case _ if self.is_numeric():
                    yield self.scan_number()
                case "\"":
                    yield self.scan_string()
                case "+" | "-" | "*" \
                         | "(" \
                         | ")" | "{" \
                         | ";" | ":" | "[" \
                         | "]" | "," | "%":
                    yield self.token(self.line, TokenType.PUNCTUATOR, self.advance())
                case "}":
                    if self.peek(1) == "}" and self.in_reference:
                        self.in_reference = False
                        return

                    yield self.token(self.line, TokenType.PUNCTUATOR, self.advance())
                case "!" | "=" | ">" | "<":
                    match self.peek() + self.peek(1):
                        case "!=" | "==" | ">=" | "<=":
                            yield self.token(self.line, TokenType.PUNCTUATOR, self.advance(2))
                        case _:
                            yield self.token(self.line, TokenType.PUNCTUATOR, self.advance())
                case "/":
                    if self.peek(1) == "*":
                        yield from self.scan_comment()
                    else:
                        yield self.token(self.line, TokenType.PUNCTUATOR, self.advance())
                case _:
                    yield self.scan_identifier()

        yield self.token(self.line, TokenType.EOF, None)

    def scan_string(self):
        self.advance()
        line = self.line
        value = ""

        c = self.peek()

        while c != "\"":
            value += self.advance()
            c = self.peek()

        self.expect("\"")
        return self.token(line, TokenType.STRING, value)

    def scan_comment(self):
        # consume '/*'
        line = self.line
        self.advance(2)
        self.consume_whitespace()

        self.expect("[")

        if self.peek() == "[":
            self.advance()
            ref = int(self.scan_number()["value"])
            self.expect("]]")

            self.consume_whitespace()
            self.expect(r"{{")
            self.in_reference = True
            self.references[ref] = list(self._lex())
            # consume '}}'
            self.advance(2)
            self.consume_whitespace()
            self.expect("*/")
        else:
            ref = int(self.scan_number()["value"])
            self.expect("]")

            while self.idx < len(self.source):
                if self.peek() == "*" and self.peek(1) == "/":
                    self.advance(2)
                    break
                else:
                    self.advance()

            yield self.token(line, TokenType.COMMENT, ref)

    def scan_number(self):
        num = ""
        while self.is_numeric():
            num += self.advance()

        if self.peek() == ".":
            num += self.advance()
            if not ("0" <= self.peek() <= "9"):
                raise Exception(f"Syntax Error: '.' on line: {self.line}")
            else:
                while "0" <= self.peek() <= "9":
                    num += self.advance()

        return self.token(self.line, TokenType.NUMBER, float(num))

    def scan_identifier(self):
        ident = ""
        line = self.line

        if self.is_alpha() or self.peek() == "$" or self.peek() == "_":
            ident += self.advance()
        else:
            raise Exception(f"Syntax Error: Invalid identifier"
                            f" start {self.peek()} on line: {self.line}")

        while self.is_alphanumeric() or self.peek() == "$" or self.peek() == "_":
            ident += self.advance()

        if self.is_keyword(ident):
            return self.token(line, TokenType.KEYWORD, ident)
        elif ident == "true" or ident == "false":
            return self.token(line, TokenType.BOOLEAN, ident == "true")
        elif ident == "null":
            return self.token(line, TokenType.NULL, None)
        else:
            return self.token(line, TokenType.IDENTIFIER, ident)

    def is_keyword(self, ident):
        match ident:
            case "if" | "while" | "return" | "let" | "function" | "else" | "print":
                return True
            case _:
                return False

    def consume_whitespace(self):
        while self.idx < len(self.source):
            match self.peek():
                case " " | "\t" | "\n" | "\r":
                    self.advance()
                case _:
                    break

    def token(self, line, token_type, value):
        return {"type": token_type,
                "value": value,
                "line": line}

    def is_alpha(self):
        x = self.peek()

        return "a" <= x <= "z" or "A" <= x <= "Z"

    def is_numeric(self):
        return "0" <= self.peek() <= "9"

    def is_alphanumeric(self):
        return self.is_alpha() or self.is_numeric()

    def expect(self, c):
        if self.advance(len(c)) != c:
            raise Exception(
                f"Syntax Error: Unexpected character(s) or EOF on line: {self.line}")

    def peek(self, n=0):
        if self.idx + n < len(self.source):
            return self.source[self.idx + n]

        return ""

    def advance(self, n=1):
        if self.idx < len(self.source):
            value = self.source[self.idx:self.idx + n]
            self.idx += n

            if value == "\n":
                self.line += 1

            return value

        raise Exception("Syntax Error: Unexpected EOF")
