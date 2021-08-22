from run_code import apply_binop, apply_comparison


class UndefinedVariableError(Exception):
    def __init__(self, varname):
        self.varname = varname

    def __str__(self):
        return f"Undefined variable: {self.varname}"


class UnknownFunctionError(Exception):
    def __init__(self, name):
        self.func_name = name

    def __str__(self):
        return f"Unknown function: {self.func_name}"


class DuplicateFunctionError(Exception):
    def __init__(self, name):
        self.func_name = name

    def __str__(self):
        return f"Duplicate function: {self.func_name}"


# All of the functions, by name.
functions = {}


def reset_functions():
    functions.clear()


def add_function(func_name: str, function):
    if func_name in functions:
        raise DuplicateFunctionError(func_name)
    functions[func_name] = function


def find_function(func_name: str):
    try:
        return functions[func_name]
    except KeyError:
        raise UnknownFunctionError(func_name)


environments = []


def reset_env():
    environments.clear()
    push_env()


def get_var(name):
    environment = environments[-1]
    try:
        return environment[name]
    except KeyError:
        raise UndefinedVariableError(name)


def set_var(name, value):
    environment = environments[-1]
    environment[name] = value


def push_env(preconfigured_env=None):
    preconfigured_env = dict() if preconfigured_env is None else preconfigured_env
    environments.append(preconfigured_env)


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
    def __init__(self, body, done=False):
        self.body = body
        print(self.body)
        self.done = done

    def __repr__(self):
        if self.done:
            return f"{self.body} AND WE'RE DONE"
        else:
            return self.body.__repr__()

    def execute(self):
        # The "done" bit is handled by Stmts class
        premature_done = self.body.execute()
        return self.done or premature_done


class StmtContents:
    pass


class IfStmt(StmtContents):
    def __init__(
        self, condition: Expr, thenpt: StmtContents, thenpt_contains_done: bool
    ):
        self.condition = condition
        self.thenpt = thenpt
        self.thenpt_contains_done = thenpt_contains_done

    def __repr__(self):
        return f"IF {self.condition} THEN {self.thenpt}"

    def execute(self):
        condition_result = self.condition.evaluate()
        if condition_result:
            self.thenpt.execute()
            return self.thenpt_contains_done


class SetStmt(StmtContents):
    def __init__(self, target: Variable, value: Expr):
        self.target = target
        self.value = value

    def __repr__(self):
        return f"SET {self.target} TO {self.value}"

    def execute(self):
        set_var(self.target, self.value.evaluate())


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
        print(f"Calling {self.func_name} with args {self.args}")
        function = find_function(self.func_name)
        evaluated_args = {name: value.evaluate() for (name, value) in self.args.items()}
        result = function.run(evaluated_args)
        set_var("it", result)
        print(f"Result of call was {result}")
        if self.postfix_assignment:
            set_var(self.postfix_assignment, result)
        if function.return_var != None:
            set_var(function.return_var, result)


class ReturnStmt(StmtContents):
    pass


class Stmts:
    def __init__(self, stmt_list):
        self.stmt_list = stmt_list
        self.length = len(stmt_list)

    def __repr__(self):
        return str(self.stmt_list)

    def execute(self):
        current_index = 0
        while current_index < self.length:
            next_stmt = self.stmt_list[current_index]
            is_done = next_stmt.execute()
            if is_done:
                break
            else:
                current_index += 1


class Function:
    def __init__(
        self,
        func_name: str,
        params: "list[Parameter]",
        body: Stmts,
        return_var: str = None,
    ):
        self.func_name = func_name
        self.params = params
        self.return_var = return_var
        self.body = body
        add_function(func_name, self)

    def __repr__(self):
        return (
            f"FUNC (NAME:{self.func_name}, PARAMS:{self.params}, "
            f"RETURNS:{self.return_var}, BODY={self.body}"
        )

    def run(self, args):
        push_env(args)
        self.body.execute()
        result = get_var(self.return_var)
        pop_env()
        return result


class Program:
    def __init__(self, funcs: "list[Function]", stmts):
        self.funcs = funcs
        self.stmts = stmts

    def __repr__(self):
        return f"\nPROGRAM \n{self.funcs}\n{self.stmts}"

    def execute(self):
        self.stmts.execute()


if __name__ == "__main__":

    from tokenise import Token

    def it_sets_variables():
        reset_env()
        set_stmt = SetStmt("x", 35)
        set_stmt.execute()
        variable = Variable("x")
        value = variable.evaluate()

        assert value == 35

    def it_pushes_and_pops_environments():
        reset_env()
        set_stmt = SetStmt("x", 17)
        set_stmt.execute()
        variable = Variable("x")
        value = variable.evaluate()
        assert value == 17

        push_env()
        set_stmt = SetStmt("x", 99)
        set_stmt.execute()
        value = variable.evaluate()
        assert value == 99

        pop_env()
        value = variable.evaluate()
        assert value == 17

    def it_complains_about_unknown_variables():
        reset_env()
        try:
            variable = Variable("x")
            variable.evaluate()
            assert False
        except UndefinedVariableError:
            assert True

    def it_complains_about_unknown_functions():
        reset_env()
        try:
            call_stmt = CallStmt("Blows the bloody doors off", [])
            call_stmt.execute()
            assert False
        except UnknownFunctionError:
            assert True

    def it_adds_functions():
        reset_functions()
        body = Stmts([])
        function = Function("Says hello", [], body)
        result = find_function("Says hello")
        assert function == result

    def it_complains_about_duplicate_functions():
        reset_functions()
        body = Stmts([])
        Function("Says hello", [], body)
        try:
            Function("Says hello", [], body)
            assert False
        except DuplicateFunctionError:
            assert True

    it_sets_variables()
    it_pushes_and_pops_environments()
    it_complains_about_unknown_variables()
    it_complains_about_unknown_functions()
    it_adds_functions()
    it_complains_about_duplicate_functions()
