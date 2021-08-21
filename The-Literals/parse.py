from run_code import apply_binop, apply_comparison
from tokenise import Token, Tokeniser


class UnexpectedTokenError(RuntimeError):
    def __init__(self, expected_token, actual_token):
        self.expected_token = expected_token
        self.actual_token = actual_token

    def __str__(self):
        return f"Expected {self.expected_token} but found {self.actual_token}"


class Parser:
    def __init__(self, get_token):
        self.get_token = get_token
        self.current_lexeme = None
        self.next_lexeme = self.get_token()

    def peek_lexeme(self):
        return self.next_lexeme

    def advance(self):
        self.current_lexeme = self.next_lexeme
        if self.current_lexeme[0] != Token.EOF:
            self.next_lexeme = self.get_token()
        return self.current_lexeme

    def expect(self, expected_token):
        token, value = self.advance()
        if token != expected_token:
            raise UnexpectedTokenError(expected_token, token)
        return (token, value)

    def parse(self):
        print(self.parse_expr())

    def parse_program(self):
        self.expect(Token.BOF)
        self.parse_stmt()
        self.expect(Token.EOF)

    def parse_stmts(self):
        stmts_end_tokens = [Token.LEAVE_FUNC, Token.EOF]
        done = False
        while not done:
            token, value = self.peek_lexeme()
            if token in stmts_end_tokens:
                done = True
            else:
                self.parse_stmt()

    def parse_stmt(self):
        self.parse_stmt_contents()
        self.expect(Token.DOT)

    def parse_stmt_contents(self):
        token, value = self.advance()
        if token == Token.SETVAR:
            self.parse_set_stmt()
        elif token == Token.IF_KEYWORD:
            self.parse_if_stmt()

    def parse_set_stmt(self):
        token, value = self.advance()
        self.parse_identifier()
        token, value = self.advance()
        if token != Token.TO:
            raise UnexpectedTokenError(Token.TO, token)
        token, value = self.advance()
        if token != Token.NUMBER:
            raise UnexpectedTokenError(Token.NUMBER, token)

    def parse_identifier(self):
        token, value = self.peek_lexeme()
        while token == Token.IDENTIFIER_WORD:
            self.advance()
            token, value = self.peek_lexeme()

    def parse_if_stmt(self):
        self.parse_expr()
        self.expect(Token.THEN)
        self.parse_stmt()
        pass

    def parse_expr(self):
        first_operand_token, first_operand_value = self.expect(Token.NUMBER)
        if first_operand_token != Token.NUMBER:
            raise UnexpectedTokenError(Token.NUMBER, first_operand_token)
        # TODO: make it work for variables.
        operator_token, operator_value = self.peek_lexeme()
        if operator_token == Token.BINOP or operator_token == Token.COMPARISON:
            return self.parse_operation(
                first_operand_value, operator_token, operator_value
            )
        else:
            return first_operand_value

    def parse_operation(self, first_operand_value, operator_token, operator_value):
        self.advance()  # we already have these values!
        second_operand_token, second_operand_value = self.advance()
        if second_operand_token != Token.NUMBER:
            raise UnexpectedTokenError(Token.NUMBER, second_operand_token)
        if operator_token == Token.BINOP:
            result = apply_binop(
                operator_value, first_operand_value, second_operand_value
            )
        if operator_token == Token.COMPARISON:
            result = apply_comparison(
                operator_value, first_operand_value, second_operand_value
            )
        return result


if __name__ == "__main__":
    # input_file = "samples/fib.comment"
    # with open(input_file, "r") as f:
    #     text = f.read()

    # tokeniser = Tokeniser(text)

    def empty_program():
        yield (Token.BOF, "")
        yield (Token.EOF, "")

    def set_x():
        yield (Token.BOF, "")
        yield (Token.SETVAR, "Set")
        yield (Token.IDENTIFIER_WORD, "x")
        yield (Token.TO, "to")
        yield (Token.NUMBER, "-39")
        yield (Token.DOT, ".")
        yield (Token.EOF, "")

    def expr():
        yield (Token.NUMBER, "-98")
        yield (Token.BINOP, "-")
        yield (Token.NUMBER, "100")
        yield (Token.EOF, "")

    def if_stmt():
        # TODO: put an if statement here to parse.
        pass

    parser = Parser(expr().__next__)
    parser.parse()
