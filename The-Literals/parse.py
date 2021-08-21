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
        return token, value

    def parse(self):
        self.parse_program()

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
        self.parse_stmt_contents()

    def parse_expr(self):
        first_operand_token, first_operand_value = self.expect(Token.NUMBER)
        # TODO: more than just a single number.
        potential_operator, next_value = self.peek_lexeme()
        if potential_operator == Token.BINOP or potential_operator == Token.COMPARISON:
            self.advance()  # we already have these values!
            second_operand_token, second_operand_value = self.advance()


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

    def if_stmt_compare_constants():
        yield (Token.BOF, "")
        yield (Token.IF_KEYWORD, "If")
        yield (Token.NUMBER, "6800")
        yield (Token.COMPARISON, "is")
        yield (Token.NUMBER, "6800")
        yield (Token.THEN, "then")
        yield (Token.SETVAR, "set")
        yield (Token.IDENTIFIER_WORD, "successor")
        yield (Token.TO, "to")
        yield (Token.NUMBER, "68000")
        yield (Token.DOT, ".")
        yield (Token.EOF, "")

    parser = Parser(set_x().__next__)
    parser.parse()

    parser = Parser(if_stmt_compare_constants().__next__)
    parser.parse()
