class StatementNode:
    class MathNode:

        def __init__(self, _type, var1, var2):
            self.type = _type
            self.var1 = var1
            self.var2 = var2

    class EqualNode:

        def __init__(self, var1, var2):
            self.var1 = var1
            self.var2 = var2

    class NotEqualNode:

        def __init__(self, var1, var2):
            self.var1 = var1
            self.var2 = var2

    class GreaterThanNode:

        def __init__(self, var1, var2):
            self.var1 = var1
            self.var2 = var2

    class GreaterOrEqualsThanNode:

        def __init__(self, var1, var2):
            self.var1 = var1
            self.var2 = var2

    class SmallerThanNode:

        def __init__(self, var1, var2):
            self.var1 = var1
            self.var2 = var2

    class SmallerOrEqualsThanNode:

        def __init__(self, var1, var2):
            self.var1 = var1
            self.var2 = var2

    class VarAssignNode:

        def __init__(self, name, value):
            self.var_name = name
            self.var_value = value

    class VarNode:

        def __init__(self, name):
            self.var_name = name

    class LiterallyNode:

        def __init__(self, var):
            self.var = var

    class FunctionDefinitionNode:

        def __init__(self, content, args):
            self.content = content
            self.args = args

    class FunctionCallNode:

        def __init__(self, func_name, args):
            self.func_name = func_name
            self.args = args
