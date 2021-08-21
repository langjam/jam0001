from application_stack_utils import StatementNode
from internals import env as internal_env, internals

env = internal_env.copy()


class ComstructExecutor:
    def __init__(self, object_tree):
        for node in object_tree:
            self.walkTree(node)

    def walkTree(self, node):
        global env

        if isinstance(node, StatementNode.MathNode):
            if node.type == "+":
                return self.walkTree(node.var1) + self.walkTree(node.var2)
            elif node.type == "-":
                return self.walkTree(node.var1) - self.walkTree(node.var2)
            elif node.type == "*":
                return self.walkTree(node.var1) * self.walkTree(node.var2)
            elif node.type == "/":
                return self.walkTree(node.var1) / self.walkTree(node.var2)
            elif node.type == "%":
                return self.walkTree(node.var1) % self.walkTree(node.var2)
        elif isinstance(node, StatementNode.EqualNode):
            return self.walkTree(node.var1) == self.walkTree(node.var2)
        elif isinstance(node, StatementNode.NotEqualNode):
            return self.walkTree(node.var1) != self.walkTree(node.var2)
        elif isinstance(node, StatementNode.GreaterThanNode):
            return self.walkTree(node.var1) > self.walkTree(node.var2)
        elif isinstance(node, StatementNode.GreaterOrEqualsThanNode):
            return self.walkTree(node.var1) >= self.walkTree(node.var2)
        elif isinstance(node, StatementNode.SmallerThanNode):
            return self.walkTree(node.var1) < self.walkTree(node.var2)
        elif isinstance(node, StatementNode.SmallerOrEqualsThanNode):
            return self.walkTree(node.var1) <= self.walkTree(node.var2)
        elif isinstance(node, StatementNode.VarAssignNode):
            env[node.var_name] = self.walkTree(node.var_value)
            return env[node.var_name]
        elif isinstance(node, StatementNode.VarNode):
            return env[node.var_name]
        elif isinstance(node, StatementNode.LiterallyNode):
            if node.walk_function is not None:
                node.walk_function(self.walkTree, node)
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

                if can_return:
                    if stmt_node:
                        ret = self.walkTree(stmt_node)

            return ret
        elif isinstance(node, StatementNode.FunctionDefinitionNode):
            return node
        elif isinstance(node, StatementNode.FunctionCallNode):
            processed_args = []
            node_call = env[node.func_name].content

            for arg in node.args:
                processed_args.append(self.walkTree(arg))
            if node_call == "internal":
                return self.walkTree(internals[node.func_name](processed_args))

            for stmt_node in node_call.content:
                if type(stmt_node) == StatementNode.FunctionDescriptionNode:
                    node_call.description = stmt_node.attributes
                    break

            if node_call.description is None:
                node_call.description = []

            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)

            old_env = env
            new_env = env.copy()
            i = 0
            for elem in node_call.description:
                if elem[0] != "param":
                    continue
                try:
                    new_env[elem[1]] = processed_args[i]
                except IndexError:
                    new_env[elem[1]] = None
                i += 1

            env = new_env

            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)
            for func_node in node_call.content:
                # TODO Check for Return Value Type, but only if defined in Desc. Only set if type matches
                ret = self.walkTree(func_node)

            env = old_env

            return ret

        elif isinstance(node, StatementNode.ForLoopExecutorNode):
            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)
            prev = None
            try:
                prev = env[node.varname]
            except LookupError:
                pass
            for i in node.tgetfrm:
                env[node.varname] = i
                ret = self.walkTree(StatementNode.ExecuteStoredProcedureNode(node.execute))

            if prev is not None:
                env[node.varname] = prev
            else:
                del env[node.varname]

            return ret
        else:
            return node
