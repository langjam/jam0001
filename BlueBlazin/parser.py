from collections import deque
from lexer import Lexer, TokenType
from enum import Enum, auto


class Ast(Enum):
    SCRIPT = auto()
    LET_DCLR = auto()
    RETURN_STMT = auto()
    BLOCK_STMT = auto()
    EXPR_STMT = auto()
    IDENTIFIER = auto()
    ASSIGNMENT = auto()
    BINARY = auto()
    UNARY = auto()
    BOOLEAN = auto()
    STRING = auto()
    NULL = auto()
    NUMBER = auto()
    DICTIONARY = auto()
    CALL = auto()
    MEMBER = auto()
    IF_STMT = auto()
    WHILE_STMT = auto()
    FUNCTION_DCLR = auto()
    PRINT_STMT = auto()
    MEMBER_ASSIGNMENT = auto()


class Parser:
    def __init__(self, source):
        self.lexer = iter(Lexer(source))
        self.line = 0
        # self.buffer = deque()
        self.buffered = None
        self.in_function = False

    def parse(self):
        body = []
        while self.peek()["type"] != TokenType.EOF:
            body.append(self.dclr())

        return {"type": Ast.SCRIPT, "body": body, "line": 0}

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
        line = self.advance()["line"]
        old_in_function = self.in_function
        self.in_function = True
        name = self.identifier()["value"]
        params = self.params()
        body = self.block()["body"]
        self.in_function = old_in_function
        return {"type": Ast.FUNCTION_DCLR, "name": name, "params": params, "body": body, "line": line}

    def params(self):
        params = []
        self.expect(TokenType.PUNCTUATOR, "(")

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": ")"} | {"type": TokenType.EOF}:
                    break
                case _:
                    params.append(self.identifier()["value"])
                    token = self.peek()
                    if token["type"] != TokenType.PUNCTUATOR or token["value"] != ")":
                        self.expect(TokenType.PUNCTUATOR, ",")

        self.expect(TokenType.PUNCTUATOR, ")")

        return params

    def let_dclr(self):
        line = self.advance()["line"]
        name = self.identifier()["value"]

        value = None
        next_token = self.peek()

        if next_token["type"] == TokenType.PUNCTUATOR and next_token["value"] == "=":
            self.advance()
            value = self.expression()

        self.expect_semicolon()

        return {"type": Ast.LET_DCLR, "name": name, "value": value, "line": line}

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
        self.expect(TokenType.PUNCTUATOR, "(")
        test = self.expression()
        self.expect(TokenType.PUNCTUATOR, ")")
        consequent = self.stmt()

        alternative = None

        token = self.peek()
        if token["type"] == TokenType.KEYWORD and token["value"] == "else":
            self.advance()
            alternative = self.stmt()

        return {"type": Ast.IF_STMT,
                "test": test,
                "consequent": consequent,
                "alternative": alternative,
                "line": line}

    def print_stmt(self):
        line = self.advance()["line"]
        value = self.expression()
        self.expect_semicolon()
        return {"type": Ast.PRINT_STMT, "value": value, "line": line}

    def while_stmt(self):
        line = self.advance()["line"]
        self.expect(TokenType.PUNCTUATOR, "(")
        test = self.expression()
        self.expect(TokenType.PUNCTUATOR, ")")
        body = self.block()["body"]

        return {"type": Ast.WHILE_STMT, "test": test, "body": body, "line": line}

    def return_stmt(self):
        line = self.advance()["line"]

        if not self.in_function:
            raise Exception(f"Illegal return on line: {line}")

        value = None
        match self.peek():
            case {"type": TokenType.PUNCTUATOR, "value": ";"}:
                self.expect_semicolon()
            case _:
                value = self.expression()
                self.expect_semicolon()

        return {"type": Ast.RETURN_STMT, "value": value, "line": line}

    def block(self):
        line = self.expect(TokenType.PUNCTUATOR, "{")
        body = []
        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": "}"} | {"type": TokenType.EOF}:
                    break
                case _:
                    body.append(self.dclr())
                    token = self.peek()

        self.expect(TokenType.PUNCTUATOR, "}")
        return {"type": Ast.BLOCK_STMT, "body": body, "line": line}

    def expr_stmt(self):
        expression = self.expression()
        self.expect_semicolon()
        return {"type": Ast.EXPR_STMT, "expression": expression, "line": expression["line"]}

    def expression(self):
        expr = self.logic_or()

        token = self.peek()
        if expr["type"] == Ast.IDENTIFIER \
                and (token["type"] == TokenType.PUNCTUATOR and token["value"] == "="):
            self.advance()
            value = self.expression()
            return {"type": Ast.ASSIGNMENT, "name": expr["value"], "value": value, "line": expr["line"]}
        elif expr["type"] == Ast.MEMBER \
                and (token["type"] == TokenType.PUNCTUATOR and token["value"] == "="):
            self.advance()
            value = self.expression()
            return {"type": Ast.MEMBER_ASSIGNMENT, "dictionary": expr["dictionary"], "key": expr["key"], "value": value, "line": expr["line"]}
        return expr

    def logic_or(self):
        expr = self.logic_and()

        token = self.peek()
        while token["type"] == TokenType.KEYWORD and token["value"] == "or":
            self.advance()
            rhs = self.logic_and()
            expr = {"type": Ast.BINARY,
                    "operator": "or",
                    "left": expr,
                    "right": rhs,
                    "line": expr["line"]}
            token = self.peek()

        return expr

    def logic_and(self):
        expr = self.equality()

        token = self.peek()
        while token["type"] == TokenType.KEYWORD and token["value"] == "and":
            self.advance()
            rhs = self.equality()
            expr = {"type": Ast.BINARY,
                    "operator": "and",
                    "left": expr,
                    "right": rhs,
                    "line": expr["line"]}
            token = self.peek()

        return expr

    def equality(self):
        expr = self.comparison()

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": "!="} | {"type": TokenType.PUNCTUATOR, "value": "=="}:
                    self.advance()
                    rhs = self.comparison()
                    expr = {"type": Ast.BINARY,
                            "operator": token["value"],
                            "left": expr,
                            "right": rhs,
                            "line": expr["line"]}
                    token = self.peek()
                case _:
                    break
        return expr

    def comparison(self):
        expr = self.term()

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": ">"} \
                        | {"type": TokenType.PUNCTUATOR, "value": ">="} \
                        | {"type": TokenType.PUNCTUATOR, "value": "<"} \
                        | {"type": TokenType.PUNCTUATOR, "value": "<="}:
                    self.advance()
                    rhs = self.term()
                    expr = {"type": Ast.BINARY,
                            "operator": token["value"],
                            "left": expr,
                            "right": rhs,
                            "line": expr["line"]}
                    token = self.peek()
                case _:
                    break
        return expr

    def term(self):
        expr = self.factor()

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": "-"} | {"type": TokenType.PUNCTUATOR, "value": "+"}:
                    self.advance()
                    rhs = self.factor()
                    expr = {"type": Ast.BINARY,
                            "operator": token["value"],
                            "left": expr,
                            "right": rhs,
                            "line": expr["line"]}
                    token = self.peek()
                case _:
                    break
        return expr

    def factor(self):
        expr = self.unary()

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": "*"} \
                        | {"type": TokenType.PUNCTUATOR, "value": "/"} \
                        | {"type": TokenType.PUNCTUATOR, "value": "%"}:
                    self.advance()
                    rhs = self.unary()
                    expr = {"type": Ast.BINARY,
                            "operator": token["value"],
                            "left": expr,
                            "right": rhs,
                            "line": expr["line"]}
                    token = self.peek()
                case _:
                    break
        return expr

    def unary(self):
        token = self.peek()

        match token:
            case {"type": TokenType.PUNCTUATOR, "value": "!"} | {"type": TokenType.PUNCTUATOR, "value": "-"}:
                argument = self.unary()
                return {"type": Ast.UNARY, "operator": token["value"], "argument": argument, "line": token["line"]}
            case _:
                return self.call()

    def call(self):
        expr = self.primary()
        line = expr["line"]

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": "("}:
                    args = self.arguments()
                    expr = {"type": Ast.CALL,
                            "callee": expr,
                            "arguments": args,
                            "line": line}
                    token = self.peek()
                case {"type": TokenType.PUNCTUATOR, "value": "["}:
                    self.advance()
                    token = self.advance()
                    match token:
                        case {"type": TokenType.NUMBER} | {"type": TokenType.STRING}:
                            expr = {"type": Ast.MEMBER,
                                    "dictionary": expr,
                                    "key": token["value"],
                                    "line": line}
                        case _:
                            raise Exception(f"Illegal key. Expected "
                                            f"Number or String on line: {token['line']}")
                    self.expect(TokenType.PUNCTUATOR, "]")
                    token = self.peek()
                case _:
                    return expr

    def arguments(self):
        self.expect(TokenType.PUNCTUATOR, "(")
        args = []

        token = self.peek()
        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": ")"} | {"type": TokenType.EOF}:
                    break
                case _:
                    args.append(self.expression())
                    token = self.peek()
                    if token["type"] != TokenType.PUNCTUATOR or token["value"] != ")":
                        self.expect(TokenType.PUNCTUATOR, ",")

        self.expect(TokenType.PUNCTUATOR, ")")
        return args

    def primary(self):
        token = self.advance()

        match token:
            case {"type": TokenType.BOOLEAN}:
                return {"type": Ast.BOOLEAN, "value": token["value"], "line": token["line"]}
            case {"type": TokenType.NULL}:
                return {"type": Ast.NULL, "value": None, "line": token["line"]}
            case {"type": TokenType.NUMBER}:
                return {"type": Ast.NUMBER, "value": token["value"], "line": token["line"]}
            case {"type": TokenType.STRING}:
                return {"type": Ast.STRING, "value": token["value"], "line": token["line"]}
            case {"type": TokenType.PUNCTUATOR, "value": "("}:
                expr = self.expression()
                self.expect(TokenType.PUNCTUATOR, ")")
                return expr
            case {"type": TokenType.PUNCTUATOR, "value": "{"}:
                return self.dictionary()
            case {"type": TokenType.IDENTIFIER, "value": value}:
                return {"type": Ast.IDENTIFIER, "value": value, "line": token["line"]}
            case _:
                raise Exception(f"Unexpected token: "
                                f"{token['value']} on line: {token['line']}")

    def identifier(self):
        token = self.advance()
        match token:
            case {"type": TokenType.IDENTIFIER, "value": value}:
                return {"type": Ast.IDENTIFIER, "value": value, "line": token["line"]}
            case _:
                raise Exception(f"Unexpected token: "
                                f"{token['value']} on line: {token['line']}")

    def dictionary(self):
        token = self.peek()
        line = token["line"]
        items = []

        while True:
            match token:
                case {"type": TokenType.PUNCTUATOR, "value": "}"}:
                    break
                case {"type": TokenType.NUMBER, "value": value}:
                    self.advance()
                    key = {"type": Ast.NUMBER,
                           "value": value, "line": token['line']}
                    self.expect(TokenType.PUNCTUATOR, ":")
                    value = self.expression()
                    items.append((key, value))
                    token = self.peek()
                    if token["type"] != TokenType.PUNCTUATOR or token["value"] != "}":
                        self.expect(TokenType.PUNCTUATOR, ",")
                        token = self.peek()
                case {"type": TokenType.STRING, "value": value}:
                    self.advance()
                    key = {"type": Ast.STRING,
                           "value": value, "line": token['line']}
                    self.expect(TokenType.PUNCTUATOR, ":")
                    value = self.expression()
                    items.append((key, value))
                    token = self.peek()

                    if token["type"] != TokenType.PUNCTUATOR or token["value"] != "}":
                        self.expect(TokenType.PUNCTUATOR, ",")
                        token = self.peek()
                case _:
                    raise Exception(f"Invalid Token: only numbers or "
                                    f"strings allowed as dictionary keys got {token['value']}")

        self.expect(TokenType.PUNCTUATOR, "}")

        return {"type": Ast.DICTIONARY, "items": items, "line": line}

    def peek(self):
        if self.buffered is not None:
            return self.buffered

        token = next(self.lexer)
        self.buffered = token
        return token

    def expect_semicolon(self):
        self.expect(TokenType.PUNCTUATOR, ";")

    def expect(self, token_type, value=None):
        token = self.advance()

        if token["type"] != token_type:
            raise Exception(f"Unexpected Token: {token['value']} "
                            f"on line {token['line']}. Expected {value or token_type}")
        if value is not None and token["value"] != value:
            raise Exception(f"Unexpected Token: {token['value']} "
                            f"on line {token['line']}. Expected {value}")
        return token

    def advance(self):
        if self.buffered is not None:
            token, self.buffered = self.buffered, None
        else:
            token = next(self.lexer)

        return token
