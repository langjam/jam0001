import application_stack_utils
from application_stack_utils import StatementNode


env = dict()


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
            return node.var_value
        elif isinstance(node, StatementNode.VarNode):
            return env[node.var_name]
        elif isinstance(node, StatementNode.LiterallyNode):
            return node.var
