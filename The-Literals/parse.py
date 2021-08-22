from abstract_syntax_trees import (
    Binop,
    Comparison,
    Function,
    IfStmt,
    Number,
    Parameter,
    Program,
    SetStmt,
    Stmts,
    Variable,
)
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
            # Swallow CODE at the start of a line or at the start of the file.
            if self.next_lexeme[0] == Token.CODE and self.current_lexeme[0] in (
                Token.EOL,
                Token.BOF,
            ):
                self.next_lexeme = self.get_token()
        return self.current_lexeme

    def expect(self, expected_token):
        token, value = self.advance()
        if token != expected_token:
            raise UnexpectedTokenError(expected_token, token)
        return token, value

    def parse(self):
        print(self.parse_program())

    def parse_program(self):
        self.expect(Token.BOF)
        functions = self.parse_functions()
        stmts = self.parse_stmts()
        self.expect(Token.EOF)
        return Program(functions, stmts)

    def parse_functions(self):
        functions = []
        token, value = self.peek_lexeme()
        while token in (Token.EOL, Token.FUNCTION):
            while token == Token.EOL:
                self.advance()
                token, value = self.peek_lexeme()

            while token == Token.FUNCTION:
                functions.append(self.parse_function())
                token, value = self.peek_lexeme()
        return functions

    def parse_function(self):
        params = []
        return_var = None

        token, value = self.advance()
        self.expect(Token.EOL)
        self.expect(Token.HFILL)
        token, func_name = self.expect(token.FUNCTION_NAME)
        self.expect(Token.DOT)
        self.expect(Token.EOL)

        token, value = self.peek_lexeme()
        while token == Token.HFILL:
            self.advance()
            token, value = self.peek_lexeme()
            if token == Token.PARAM:
                params.append(self.parse_param())
            elif token == Token.EOL:
                self.advance()
            elif token == Token.RETURNVAR:
                return_var = self.parse_returnvar()
                token, value = self.peek_lexeme()
                while token == Token.HFILL:
                    self.advance()
                    self.expect(Token.EOL)
                    token, value = self.peek_lexeme()
                break
            token, value = self.peek_lexeme()

        # End of function header.
        self.expect(Token.HEADER_END)
        self.expect(Token.EOL)

        # Parse the function body.
        body = self.parse_stmts()

        # End of function definition.
        self.expect(Token.END_DEF)
        self.expect(Token.DOT)
        self.expect(Token.EOL)

        return Function(func_name, params, return_var, body)

    def parse_param(self):
        self.advance()
        param_name = self.parse_identifier()
        self.expect(Token.EOL)
        return Parameter(param_name)

    def parse_returnvar(self):
        self.advance()
        varname = self.parse_identifier()
        self.expect(Token.EOL)
        return Variable(varname)

    def parse_stmts(self):
        stmts_end_tokens = [Token.END_DEF, Token.EOF]
        stmts = []
        done = False
        while not done:
            token, value = self.peek_lexeme()
            if token in stmts_end_tokens:
                done = True
            else:
                stmts.append(self.parse_stmt())
                token, value = self.peek_lexeme()
                while token == Token.EOL:
                    self.advance()
                    token, value = self.peek_lexeme()
        return Stmts(stmts)

    def parse_stmt(self):
        contents = self.parse_stmt_contents()
        token, value = self.peek_lexeme()
        if token == Token.LEAVE_FUNC:
            self.advance()
        self.expect(Token.DOT)
        return contents

    def parse_stmt_contents(self):
        token, value = self.advance()
        if token == Token.SETVAR:
            return self.parse_set_stmt()
        elif token == Token.IF_KEYWORD:
            return self.parse_if_stmt()

    def parse_set_stmt(self):
        target = self.parse_identifier()
        token, value = self.advance()
        if token != Token.TO:
            raise UnexpectedTokenError(Token.TO, token)
        expr = self.parse_expr()
        return SetStmt(target, expr)

    def parse_identifier(self):
        identifier_words = []
        token, value = self.peek_lexeme()
        while token == Token.IDENTIFIER_WORD:
            identifier_words.append(value)
            self.advance()
            token, value = self.peek_lexeme()
        return " ".join(identifier_words)

    def parse_if_stmt(self):
        condition = self.parse_expr()
        self.expect(Token.THEN)
        stmt = self.parse_stmt_contents()
        return IfStmt(condition, stmt)

    def parse_expr(self):
        first_operand = self.parse_operand()
        operator_token, operator_value = self.peek_lexeme()
        if operator_token == Token.BINOP or operator_token == Token.COMPARISON:
            self.advance()  # we already have these values!
            second_operand = self.parse_operand()
            if operator_token == Token.BINOP:
                return Binop(operator_value, first_operand, second_operand)
            if operator_token == Token.COMPARISON:
                return Comparison(operator_value, first_operand, second_operand)
        else:
            return first_operand

    def parse_operand(self):
        token, value = self.peek_lexeme()
        if token == Token.NUMBER:
            operand_token, operand_value = self.expect(Token.NUMBER)
            return Number(operand_value)
        elif token == Token.IDENTIFIER_WORD:
            varname = self.parse_identifier()
            return Variable(varname)
        else:
            raise UnexpectedTokenError(
                Token.IDENTIFIER_WORD, token
            )  # TODO: could be either.


if __name__ == "__main__":

    def set_var_to_constant():
        yield (Token.SETVAR, "Set")
        yield (Token.IDENTIFIER_WORD, "x")
        yield (Token.TO, "to")
        yield (Token.NUMBER, "-39")
        yield (Token.DOT, ".")

    def expr():
        yield (Token.NUMBER, "-98")
        yield (Token.BINOP, "-")
        yield (Token.NUMBER, "100")
        yield (Token.EOF, "")

    def set_var_to_var():
        yield (Token.SETVAR, "Set")
        yield (Token.IDENTIFIER_WORD, "retirement")
        yield (Token.IDENTIFIER_WORD, "age")
        yield (Token.TO, "to")
        yield (Token.IDENTIFIER_WORD, "pension")
        yield (Token.IDENTIFIER_WORD, "age")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")

    def set_var_to_var_binop():
        yield (Token.SETVAR, "Set")
        yield (Token.IDENTIFIER_WORD, "the")
        yield (Token.IDENTIFIER_WORD, "number")
        yield (Token.TO, "to")
        yield (Token.IDENTIFIER_WORD, "the")
        yield (Token.IDENTIFIER_WORD, "number")
        yield (Token.BINOP, "*")
        yield (Token.NUMBER, "2")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")

    def if_stmt_compare_constants():
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

    def if_stmt_compare_variable_and_constant():
        yield (Token.IF_KEYWORD, "If")
        yield (Token.IDENTIFIER_WORD, "processor")
        yield (Token.IDENTIFIER_WORD, "type")
        yield (Token.COMPARISON, "is")
        yield (Token.NUMBER, "6800")
        yield (Token.THEN, "then")
        yield (Token.SETVAR, "set")
        yield (Token.IDENTIFIER_WORD, "processor")
        yield (Token.IDENTIFIER_WORD, "type")
        yield (Token.TO, "to")
        yield (Token.NUMBER, "68000")
        yield (Token.DOT, ".")

    def if_stmt_compare_variable_and_variable():
        yield (Token.IF_KEYWORD, "If")
        yield (Token.IDENTIFIER_WORD, "processor")
        yield (Token.IDENTIFIER_WORD, "type")
        yield (Token.COMPARISON, "is")
        yield (Token.IDENTIFIER_WORD, "eight")
        yield (Token.IDENTIFIER_WORD, "bit")
        yield (Token.THEN, "then")
        yield (Token.SETVAR, "set")
        yield (Token.IDENTIFIER_WORD, "processor")
        yield (Token.IDENTIFIER_WORD, "architecture")
        yield (Token.TO, "to")
        yield (Token.NUMBER, "6502")
        yield (Token.DOT, ".")

    def if_stmt_compare_constants_and_leave():
        yield (Token.IF_KEYWORD, "If")
        yield (Token.NUMBER, "6800")
        yield (Token.COMPARISON, "is")
        yield (Token.NUMBER, "6800")
        yield (Token.THEN, "then")
        yield (Token.SETVAR, "set")
        yield (Token.IDENTIFIER_WORD, "successor")
        yield (Token.TO, "to")
        yield (Token.NUMBER, "68000")
        yield (Token.LEAVE_FUNC, "and we're done")
        yield (Token.DOT, ".")

    def function_no_params_no_return():
        yield (Token.FUNCTION, "/**")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.FUNCTION_NAME, "Opens the pod bay doors")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")
        yield (Token.HEADER_END, "*/")
        yield (Token.EOL, "\n")
        yield (Token.END_DEF, "And we're done")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")

    def function_params_no_return():
        yield (Token.FUNCTION, "/**")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.FUNCTION_NAME, "Multiplies by two")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.PARAM, "@param")
        yield (Token.IDENTIFIER_WORD, "the")
        yield (Token.IDENTIFIER_WORD, "number")
        yield (Token.EOL, "\n")
        yield (Token.HEADER_END, "*/")
        yield (Token.EOL, "\n")
        yield from set_var_to_var_binop()
        yield (Token.END_DEF, "And we're done")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")

    def function_params_and_return():
        yield (Token.FUNCTION, "/**")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.FUNCTION_NAME, "Multiplies by two")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.PARAM, "@param")
        yield (Token.IDENTIFIER_WORD, "the")
        yield (Token.IDENTIFIER_WORD, "number")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.RETURNVAR, "@return")
        yield (Token.IDENTIFIER_WORD, "the")
        yield (Token.IDENTIFIER_WORD, "result")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.EOL, "\n")
        yield (Token.HFILL, " *")
        yield (Token.EOL, "\n")
        yield (Token.HEADER_END, "*/")
        yield (Token.EOL, "\n")
        yield from set_var_to_var_binop()
        yield from set_var_to_var_binop()
        yield from set_var_to_var_binop()
        yield (Token.END_DEF, "And we're done")
        yield (Token.DOT, ".")
        yield (Token.EOL, "\n")

    def code():
        yield (Token.CODE, "//")

    def program(*fragments):
        def generator():
            yield (Token.BOF, "")
            for fragment in fragments:
                yield from fragment()
            yield (Token.EOF, "")

        return generator().__next__

    parser = Parser(program())
    parser.parse()

    parser = Parser(program(set_var_to_constant))
    parser.parse()

    parser = Parser(program(set_var_to_var))
    parser.parse()

    parser = Parser(program(set_var_to_var_binop))
    parser.parse()

    parser = Parser(program(if_stmt_compare_constants))
    parser.parse()

    parser = Parser(program(if_stmt_compare_variable_and_constant))
    parser.parse()

    parser = Parser(program(if_stmt_compare_variable_and_variable))
    parser.parse()

    parser = Parser(program(if_stmt_compare_constants_and_leave))
    parser.parse()

    parser = Parser(
        program(if_stmt_compare_constants, if_stmt_compare_variable_and_constant)
    )
    parser.parse()

    parser = Parser(program(function_no_params_no_return))
    parser.parse()

    parser = Parser(program(function_params_no_return))
    parser.parse()

    parser = Parser(program(function_params_and_return))
    parser.parse()

    parser = Parser(program(code, set_var_to_constant))
    parser.parse()

    input_file = "samples/fib.comment"
    with open(input_file, "r") as f:
        text = f.read()

    tokeniser = Tokeniser(text)
    parser = Parser(tokeniser.tokenise().__next__)
    parser.parse()
