from collections import deque
from .lexer import Lexer, TokenType
from enum import Enum


class Ast(Enum):
    SCRIPT = 0
    LET_DCLR = 1


class Parser:
    def __init__(self, source):
        self.lexer = iter(Lexer(source))
        self.line = 0
        self.buffer = deque()
        self.in_function = False

    def parse(self):
        body = []
        while self.peek()["type"] != TokenType.EOF:
            body.append(self.dclr())

        return {"type": Ast.SCRIPT, "body": body}

    def dclr(self):
        token = self.peek()
        match token:
            case {"type": TokenType.KEYWORD, "value": "function"}:
                return self.function_dclr()
            case {"type": TokenType.KEYWORD, "value": "let"}:
                return self.let_dclr()
            case _:
                return self.stmt()

    def function_dclr(self):
        pass

    def let_dclr(self):
        line = self.advance()["line"]
        name = self.expect(TokenType.IDENTIFIER)["value"]

        value = None
        next_token = self.peek()

        if next_token.type == TokenType.PUNCTUATOR and next_token.value == "=":
            self.advance()
            value = self.expr()

        return {"type": Ast.LET_DCLR, "name": name, "value": value}

    def stmt(self):
        token = self.peek()
        match token:
            case {"type": TokenType.KEYWORD, "value": "if"}:
                return self.if_stmt()
            case {"type": TokenType.KEYWORD, "value": "print"}:
                return self.print_stmt()
            case {"type": TokenType.KEYWORD, "value": "while"}:
                return self.while_stmt()
            case {"type": TokenType.KEYWORD, "value": "return"}:
                return self.return_stmt()
            case {"type": TokenType.PUNCTUATOR, "value": "{"}:
                return self.block()
            case _:
                return self.expr_stmt()

    def if_stmt(self):
        line = self.advance()["line"]

    def print_stmt(self):
        line = self.advance()["line"]

    def while_stmt(self):
        line = self.advance()["line"]

    def return_stmt(self):
        line = self.advance()["line"]

    def block(self):
        line = self.advance()["line"]

    def expr_stmt(self):
        line = self.advance()["line"]

    def peek(self):
        token = next(self.lexer)
        self.buffer.append(token)
        return token

    def expect_semicolon(self):
        self.expect(TokenType.PUNCTUATOR, ";")

    def expect(self, token_type, value=None):
        token = self.advance()
        if token["type"] != token_type:
            raise Exception(f"Unexpected Token: {token['value']} "
                            f"on line {token['line']}. Expected type {token_type}")
        if value is not None and token["value"] != value:
            raise Exception(f"Unexpected Token: {token['value']} "
                            f"on line {token['line']}. Expected {value}")
        return token

    def advance(self):
        if len(self.buffer) > 0:
            token = self.buffer.popleft()
        else:
            token = next(self.lexer)

        self.line = token.line
        return token
