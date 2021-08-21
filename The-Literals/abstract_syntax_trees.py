class Expr:
    pass

class Operand(Expr):
    pass

class Number(Operand):
    def __init__(self, value: int):
        self.value = value

    def __repr__(self):
        return f"NUMBER {self.value}"

class Variable(Operand):
    def __init__(self, varname: str):
        self.varname = varname

    def __repr__(self):
        return f"VAR {self.varname}"

class Parameter(Expr):
    def __init__(self, varname: str):
        self.varname = varname

    def __repr__(self):
        return f"PARAM {self.varname}"

class Binop(Expr):
    def __init__(self, operator: str, left_operand: Operand, right_operand: Operand):
        self.operator = operator
        self.left_operand = left_operand
        self.right_operand = right_operand

    def __repr__(self):
        return f"({self.left_operand}) {self.operator} ({self.right_operand})"


class Comparison(Expr):
    def __init__(self, operator: str, left_operand: Operand, right_operand: Operand):
        self.operator = operator
        self.left_operand = left_operand
        self.right_operand = right_operand

    def __repr__(self):
        return f"({self.left_operand}) {self.operator} ({self.right_operand})"


class Stmt:
    pass


class IfStmt(Stmt):
    def __init__(self, condition: Expr, thenpt: Stmt):
        self.condition = condition
        self.thenpt = thenpt

    def __repr__(self):
        return f"IF {self.condition} THEN {self.thenpt}"


class SetStmt(Stmt):
    def __init__(self, target: Variable, value: Expr):
        self.target = target
        self.value = value

    def __repr__(self):
        return f"SET {self.target} TO {self.value}"

class FuncHeader:
    def __init__(self, func_name: str, params: "list[Parameter]", return_var: Variable):
        self.func_name = func_name
        self.params = params
        self.return_var = return_var

    def __repr__(self):
        return (
            f"FUNC (NAME:{self.func_name}, PARAMS:{self.params}, RETURNS:{self.return_var}")

class Program:
    def __init__(self, funcs: "list[FuncHeader]", stmts):
        self.funcs = funcs
        self.stmts = stmts

    def __repr__(self):
        return (
            f"\nPROGRAM \n{self.funcs}\n{self.stmts}")