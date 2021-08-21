from application_stack_utils import StatementNode
from internals import env as internal_env, internals

env = internal_env


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
            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)
            for stmt_node in node.exec.content:
                ret = self.walkTree(stmt_node)
            return ret
        elif isinstance(node, StatementNode.FunctionDefinitionNode):
            return node
        elif isinstance(node, StatementNode.FunctionCallNode):
            processed_args = []
            for arg in node.args:
                processed_args.append(self.walkTree(arg))
            if env[node.func_name].content == "internal":
                return self.walkTree(internals[node.func_name](processed_args))
            ret: StatementNode.GenericNode = StatementNode.LiterallyNode(0)
            for func_node in env[node.func_name].content.content:
                ret = self.walkTree(func_node)
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
