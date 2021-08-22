from run_code import apply_binop, apply_comparison

defined_variables = {}


class undefinedVariableError(Exception):
    def __init__(self, varname):
        self.varname = varname

    def __str__(self):
        return f"Undefined variable: {self.varname}"


class Expr:
    pass


class Operand(Expr):
    pass


class Number(Operand):
    def __init__(self, value: int):
        self.value = value

    def __repr__(self):
        return f"NUMBER {self.value}"

    def evaluate(self):
        return self.value


class Variable(Operand):
    def __init__(self, varname: str):
        self.varname = varname

    def __repr__(self):
        return f"VAR {self.varname}"

    def evaluate(self):
        if self.varname in defined_variables:
            return defined_variables[self.varname]
        else:
            raise undefinedVariableError(self.varname)


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

    def evaluate(self):
        return apply_binop(self.operator, self.left_operand, self.right_operand)


class Comparison(Expr):
    def __init__(self, operator: str, left_operand: Operand, right_operand: Operand):
        self.operator = operator
        self.left_operand = left_operand
        self.right_operand = right_operand

    def __repr__(self):
        return f"({self.left_operand}) {self.operator} ({self.right_operand})"

    def evaluate(self):
        return apply_comparison(self.operator, self.left_operand, self.right_operand)


class Stmt:
    pass


class IfStmt(Stmt):
    def __init__(self, condition: Expr, thenpt: Stmt):
        self.condition = condition
        self.thenpt = thenpt

    def __repr__(self):
        return f"IF {self.condition} THEN {self.thenpt}"

    def execute(self):
        condition_result = self.condition.evaluate()
        if condition_result:
            self.thenpt.execute()


class SetStmt(Stmt):
    def __init__(self, target: Variable, value: Expr):
        self.target = target
        self.value = value

    def __repr__(self):
        return f"SET {self.target} TO {self.value}"

    def execute(self):
        defined_variables[self.target] = self.value

class Function:
    def __init__(self, func_name: str, params: "list[Parameter]", return_var: Variable, body: Stmts):
        self.func_name = func_name
        self.params = params
        self.return_var = return_var
        self.body = body

    def __repr__(self):
        return (
            f"FUNC (NAME:{self.func_name}, PARAMS:{self.params}, "
            f"RETURNS:{self.return_var}, BODY={self.body}"
        )


class Stmts:
    def __init__(self, stmt_list):
        self.stmt_list = stmt_list
        self.current_index = 0
        self.length = len(stmt_list)

    def __repr__(self):
        return str(self.stmt_list)

    def execute(self):
        while self.current_index < self.length:
            next_stmt = self.stmt_list[self.current_index]
            if (
                True
            ):  # TODO: build in a check here to ensure not a JUMP stmt or return from function
                next_stmt.execute()
            self.current_index += 1

class Program:
    def __init__(self, funcs: "list[Function]", stmts):
        self.funcs = funcs
        self.stmts = stmts

    def __repr__(self):
        return f"\nPROGRAM \n{self.funcs}\n{self.stmts}"

