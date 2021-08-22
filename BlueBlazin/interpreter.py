from parser import Ast


class Env:
    def __init__(self, parent):
        self.parent = parent
        self.table = {}

    def get(self, key):
        env = self
        while env is not None:
            if key in env.table:
                return env.table[key]
            env = env.parent
        return (None,)

    def set_value(self, key, value):
        self.table[key] = value

    def assign_value(self, key, value):
        env = self
        while env is not None:
            if key in env.table:
                env.table[key] = value
                return
            env = env.parent
        raise Exception(f"Runtime Error: {key} not found")


class Fun:
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.arity = len(params)
        self.body = body


class Return(Exception):
    def __init__(self, value):
        self.value = value


class Interpreter:
    def __init__(self, ast):
        self.ast = ast
        self.env = Env(None)

    def run(self):
        for stmt_or_dclr in self.ast["body"]:
            self.evaluate(stmt_or_dclr)

    def evaluate(self, stmt_or_dclr):
        match stmt_or_dclr["type"]:
            case Ast.EXPR_STMT:
                self.expression(stmt_or_dclr["expression"])
            case Ast.PRINT_STMT:
                value = self.expression(stmt_or_dclr["value"])
                print(value)
            case Ast.LET_DCLR:
                key = stmt_or_dclr["name"]
                value = self.expression(stmt_or_dclr["value"])
                self.env.set_value(key, value)
            case Ast.BLOCK_STMT:
                self.block(stmt_or_dclr)
            case Ast.IF_STMT:
                self.if_stmt(stmt_or_dclr)
            case Ast.WHILE_STMT:
                self.while_stmt(stmt_or_dclr)
            case Ast.FUNCTION_DCLR:
                self.function(stmt_or_dclr)
            case Ast.RETURN_STMT:
                raise Return(self.expression(stmt_or_dclr["value"]))
            case _:
                raise Exception(f"Internal Error {stmt_or_dclr}")

    def block(self, block):
        self.env = Env(self.env)

        try:
            for stmt_or_dclr in block["body"]:
                self.evaluate(stmt_or_dclr)
        except Return as e:
            self.env = self.env.parent
            raise e

        self.env = self.env.parent

    def function(self, expr):
        name = expr["name"]
        function = Fun(name, expr["params"], expr["body"])
        self.env.set_value(name, function)

    def if_stmt(self, expr):
        if self.expression(expr["test"]):
            self.evaluate(expr["consequent"])
        elif expr["alternative"] is not None:
            self.evaluate(expr["alternative"])

    def while_stmt(self, expr):
        self.env = Env(self.env)

        try:
            while self.expression(expr["test"]):
                for stmt_or_dclr in expr["body"]:
                    self.evaluate(stmt_or_dclr)
        except Return as e:
            self.env = self.env.parent
            raise e

        self.env = self.env.parent

    def expression(self, expr):
        match expr["type"]:
            case Ast.BINARY:
                return self.binary(expr)
            case Ast.UNARY:
                return self.unary(expr)
            case Ast.NUMBER | Ast.BOOLEAN | Ast.NULL | Ast.STRING:
                return expr["value"]
            case Ast.DICTIONARY:
                items = map(lambda x: map(self.expression, x), expr["items"])
                return dict(items)
            case Ast.IDENTIFIER:
                name = expr["value"]

                value = self.env.get(name)
                if value == (None,):
                    raise Exception(f"Runtime Error: identifier {name} "
                                    f"not found on line: {expr['line']}")
                return value
            case Ast.CALL:
                return self.call(expr)
            case Ast.ASSIGNMENT:
                key = expr["name"]
                value = self.expression(expr["value"])
                self.env.assign_value(key, value)
                return value
            case Ast.MEMBER_ASSIGNMENT:
                key = expr["key"]
                dictionary = self.expression(expr["dictionary"])
                if not isinstance(dictionary, dict):
                    raise Exception(
                        f"Runtime Error: object is not a dictionary")
                value = self.expression(expr["value"])
                dictionary[key] = value
                return value
            case Ast.MEMBER:
                dictionary = self.expression(expr["dictionary"])
                if not isinstance(dictionary, dict):
                    raise Exception(
                        f"Runtime Error: object is not a dictionary")
                # key = self.expression(expr["key"])
                key = expr["key"]
                return dictionary.get(key, None)
            case _:
                raise Exception(f"Internal Error {expr}")

    def call(self, expr):
        function = self.expression(expr["callee"])

        if not isinstance(function, Fun):
            raise Exception(f"Runtime Error: {function} is not callable")

        if len(expr["arguments"]) != function.arity:
            raise Exception(f"Runtime Error: incorrect number "
                            f"of arguments provided to function {function.name}")

        args = [self.expression(arg) for arg in expr["arguments"]]

        self.env = Env(self.env)

        for param, arg in zip(function.params, args):
            self.env.set_value(param, arg)

        return_value = None

        try:
            for stmt_or_dclr in function.body:
                self.evaluate(stmt_or_dclr)
        except Return as e:
            return_value = e.value

        self.env = self.env.parent
        return return_value

    def unary(self, expr):
        value = self.expression(expr["argument"])

        match expr["operator"]:
            case "-":
                return -value
            case "!":
                return not value
            case _:
                raise Exception("Internal Error")

    def binary(self, expr):
        lhs = self.expression(expr["left"])
        rhs = self.expression(expr["right"])

        match expr["operator"]:
            case "+":
                return lhs + rhs
            case "-":
                return lhs - rhs
            case "*":
                return lhs * rhs
            case "/":
                return lhs / rhs
            case ">":
                return lhs > rhs
            case ">=":
                return lhs >= rhs
            case "<":
                return lhs < rhs
            case "<=":
                return lhs <= rhs
            case "!=":
                return lhs != rhs
            case "==":
                return lhs == rhs
            case "and":
                return lhs and rhs
            case "or":
                return lhs or rhs
            case "%":
                return lhs % rhs
            case _:
                raise Exception("Internal Error")
