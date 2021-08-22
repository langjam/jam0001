from run_code import apply_binop, apply_comparison


class UndefinedVariableError(Exception):
    def __init__(self, varname):
        self.varname = varname

    def __str__(self):
        return f"Undefined variable: {self.varname}"


# All of the functions, by name.
# Q. Would builtins go here also?
functions = {}


environments = []


def reset_env():
    environments.clear()
    push_env()  # This may not be necessary.


def get_var(name):
    environment = environments[-1]
    try:
        return environment[name]
    except KeyError:
        raise UndefinedVariableError(name)


def set_var(name, value):
    environment = environments[-1]
    environment[name] = value


def push_env():
    environments.append(dict())


def pop_env():
    environments.pop()


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
        return get_var(self.varname)


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
        set_var(self.target, self.value)

def find_function(func_name: str):
    # Move this later, and also define it!
    pass

class CallStmt(Stmt):
    def __init__(self, func_name, args, postfix_assignment=None):
        self.func_name = func_name
        self.args = args
        self.postfix_assignment = postfix_assignment

    def __repr__(self):
        if self.postfix_assignment:
            return f"CALL {self.func_name} WITH {self.args} AND CALL IT {self.postfix_assignment}"
        else:
            return f"CALL {self.func_name} WITH {self.args}"

    def execute(self):
        function = find_function(self.func_name)
        return_val = function.run(self.args)
        if self.postfix_assignment:
            # TODO: Assign some_env[postfix_assignment] = return_val
            pass

class ReturnStmt(Stmt):
    pass

class CallStmt(Stmt):
    def __init__(self, func_name, args, postfix_assignment=None):
        self.func_name = func_name
        self.args = args
        self.postfix_assignment = postfix_assignment

    def __repr__(self):
        if self.postfix_assignment:
            return f"CALL {self.func_name} WITH {self.args} AND CALL IT {self.postfix_assignment}"
        else:
            return f"CALL {self.func_name} WITH {self.args}"


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
            next_stmt.execute()
            if isinstance(next_stmt, ReturnStmt):
                break
            else:
                self.current_index += 1


class Function:
    def __init__(
        self,
        func_name: str,
        params: "list[Parameter]",
        return_var: Variable,
        body: Stmts,
    ):
        self.func_name = func_name
        self.params = params
        self.return_var = return_var
        self.body = body

    def __repr__(self):
        return (
            f"FUNC (NAME:{self.func_name}, PARAMS:{self.params}, "
            f"RETURNS:{self.return_var}, BODY={self.body}"
        )

    def run(self):
        self.body.execute()


class Program:
    def __init__(self, funcs: "list[Function]", stmts):
        self.funcs = funcs
        self.stmts = stmts

    def __repr__(self):
        return f"\nPROGRAM \n{self.funcs}\n{self.stmts}"
