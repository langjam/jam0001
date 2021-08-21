class Expr:
    pass


class Number(Expr):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"NUMBER {self.value}"


class Variable(Expr):
    def __init__(self, varname):
        self.varname = varname

    def __repr__(self):
        return f"VAR {self.varname}"

class Parameter(Expr):
    def __init__(self, varname):
        self.varname = varname

    def __repr__(self):
        return f"PARAM {self.varname}"

class Binop(Expr):
    def __init__(self, operator, left_operand, right_operand):
        self.operator = operator
        self.left_operand = left_operand
        self.right_operand = right_operand

    def __repr__(self):
        return f"({self.left_operand}) {self.operator} ({self.right_operand})"


class Comparison(Expr):
    def __init__(self, operator, left_operand, right_operand):
        self.operator = operator
        self.left_operand = left_operand
        self.right_operand = right_operand

    def __repr__(self):
        return f"({self.left_operand}) {self.operator} ({self.right_operand})"


class Stmt:
    pass


class IfStmt(Stmt):
    def __init__(self, condition, thenpt):
        self.condition = condition
        self.thenpt = thenpt

    def __repr__(self):
        return f"IF {self.condition} THEN {self.thenpt}"


class SetStmt(Stmt):
    def __init__(self, target, value):
        self.target = target
        self.value = value

    def __repr__(self):
        return f"SET {self.target} TO {self.value}"


class Program:
    def __init__(self, funcs, stmts):
        self.funcs = funcs
        self.stmts = stmts

    def __repr__(self):
        return (
            f"\nPROGRAM \n{self.funcs}\n{self.stmts}")

class FuncHeader:
    def __init__(self, func_name, params, return_var):
        self.func_name = func_name
        self.params = params
        self.return_var = return_var

    def __repr__(self):
        return (
            f"FUNC (NAME:{self.func_name}, PARAMS:{self.params}, RETURNS:{self.return_var}")