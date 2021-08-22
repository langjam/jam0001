from application_stack_utils import StatementNode
from internals import env as internal_env, internals
import uuid

env = internal_env.copy()


class ComstructExecutor:
    def __init__(self, object_tree):
        global env
        for node in object_tree:
            self.walkTree(node, env)

    def walkTree(self, node, _env):

        if isinstance(node, StatementNode.MathNode):
            if node.type == "+":
                return self.walkTree(node.var1, _env) + self.walkTree(node.var2, _env)

            elif node.type == "-":
                return self.walkTree(node.var1, _env) - self.walkTree(node.var2, _env)

            elif node.type == "*":
                return self.walkTree(node.var1, _env) * self.walkTree(node.var2, _env)

            elif node.type == "/":
                return self.walkTree(node.var1, _env) / self.walkTree(node.var2, _env)

            elif node.type == "%":
                return self.walkTree(node.var1, _env) % self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.EqualNode):
            return self.walkTree(node.var1, _env) == self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.NotEqualNode):
            return self.walkTree(node.var1, _env) != self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.GreaterThanNode):
            return self.walkTree(node.var1, _env) > self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.GreaterOrEqualsThanNode):
            return self.walkTree(node.var1, _env) >= self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.SmallerThanNode):
            return self.walkTree(node.var1, _env) < self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.SmallerOrEqualsThanNode):
            return self.walkTree(node.var1, _env) <= self.walkTree(node.var2, _env)

        elif isinstance(node, StatementNode.VarAssignNode):
            _env[node.var_name] = self.walkTree(node.var_value, _env)
            return _env[node.var_name]

        elif isinstance(node, StatementNode.VarNode):
            return _env[node.var_name]

        elif isinstance(node, StatementNode.LiterallyNode):
            if node.walk_function is not None:
                node.walk_function(lambda var0: self.walkTree(var0, _env), node)
            return node.var

        elif isinstance(node, StatementNode.StoredProcedureNode):
            return node

        elif isinstance(node, StatementNode.ExecuteStoredProcedureNode):
            for stmt_node in node.exec.content:
                if type(stmt_node) == StatementNode.FunctionDescriptionNode:
                    node.description = stmt_node.attributes
                    break

            if node.description is None:
                node.description = []

            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)

            for stmt_node in node.exec.content:
                can_return = False
                for item in node.description:
                    if item[0] == "returns":
                        can_return = True
                        break

                if stmt_node:
                    ret_evt = self.walkTree(stmt_node, _env)

                    if can_return:
                        ret = ret_evt

            return ret

        elif isinstance(node, StatementNode.FunctionDefinitionNode):
            return node

        elif isinstance(node, StatementNode.ClassDefinitionNode):
            return node

        elif isinstance(node, StatementNode.ClassInstanciationNode):
            clazz = _env[node.class_name]

            return ClassClass(uuid.uuid4().hex, _env, clazz.content.content, self, node.args)

        elif isinstance(node, StatementNode.FunctionCallNode):
            processed_args = []
            node_call = _env[node.func_name].content

            for arg in node.args:
                processed_args.append(self.walkTree(arg, _env))

            if node_call == "internal":
                return self.walkTree(internals[node.func_name](processed_args), _env)

            argslist, can_return = prepareDescription(node_call.content, processed_args)

            if type(_env[node.func_name]) == StatementNode.ClassDefinitionNode:
                return self.walkTree(StatementNode.ClassInstanciationNode(node.func_name, processed_args), _env)

            backup = {}
            to_delete = []
            new_env: dict = _env.copy()

            for arg in argslist:
                try:
                    backup[arg[0]] = new_env[arg[0]] if arg[1] is not None else None
                except KeyError:
                    to_delete.append(arg[0])

                new_env[arg[0]] = arg[1]

            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(None)

            for func_node in node_call.content:
                ret_evt = self.walkTree(func_node, new_env)

                if can_return:
                    ret = ret_evt

            for i in to_delete:
                del new_env[i]

            checker_env: dict = _env.copy()

            for it in new_env.copy().items():
                try:
                    if new_env[it[0]] == checker_env[it[0]]:
                        del new_env[it[0]]
                except KeyError:
                    pass

            return ret

        elif isinstance(node, StatementNode.ForLoopExecutorNode):
            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)
            prev = None
            try:
                prev = _env[node.varname]
            except LookupError:
                pass
            for i in node.tgetfrm:
                _env[node.varname] = i
                ret = self.walkTree(StatementNode.ExecuteStoredProcedureNode(node.execute), _env)

            if prev is not None:
                _env[node.varname] = prev
            else:
                del _env[node.varname]

            return ret
        else:
            return node


class ClassClass:
    def __init__(self, uid, environment, calls, executor, args):
        self.calls: list = calls
        self.uid = uid
        self.env = environment

        self.__runConstructor(args, executor)

    def __runConstructor(self, args, executor: ComstructExecutor):
        for node in self.calls:
            if type(node) == StatementNode.VarAssignNode and node.var_name == "comment":
                self.__runFunc("comment", args, executor)
                return

    def __runFunc(self, m, args, executor: ComstructExecutor):
        for node in self.calls:
            if type(node) == StatementNode.VarAssignNode and node.var_name == m:
                contents = self.env[node.var_value.func_name].content

                try:
                    c = self.env[self.uid]
                except KeyError:
                    self.env[self.uid] = dict()

                backup = {}
                to_delete = []
                new_env: dict = {**self.env.copy(), **self.env[self.uid]}

                for param in prepareDescription(contents, args)[0]:
                    try:
                        backup[param[0]] = self.env[param[0]]
                    except KeyError:
                        to_delete.append(param[0])  # Argument not prev. defined as a variable, so deleted after exec

                    new_env[param[0]] = param[1]

                executor.walkTree(
                    StatementNode.ExecuteStoredProcedureNode(
                        StatementNode.StoredProcedureNode(
                            contents
                        )
                    ),
                    new_env
                )

                for param in to_delete:
                    del new_env[param]

                checker_env: dict = {**self.env.copy(), **self.env[self.uid]}

                for it in new_env.copy().items():
                    if new_env[it[0]] == checker_env[it[0]]:
                        del new_env[it[0]]

                print(f"F(): {new_env}")

                self.env[self.uid] = {**self.env[self.uid], **new_env}

                return

        raise ElementNotFoundException(f"Function {m} not found!")


class ElementNotFoundException(Exception):
    pass


def prepareDescription(nodes: list, args: list):
    description = []
    can_return = False

    for stmt in nodes:
        if type(stmt) != StatementNode.FunctionDescriptionNode:
            continue

        argi = 0

        stmt: StatementNode.FunctionDescriptionNode
        for attrib in stmt.attributes:
            if attrib[0] == "param":
                description.append([attrib[1], args[argi]] if len(args) >= argi + 1 else None)
                argi += 1
            else:
                can_return = True

        break

    return description, can_return
