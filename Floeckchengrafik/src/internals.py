from application_stack_utils import StatementNode


def out(args):
    for i in args:
        print(i, end=" ")

    print()


def when(args):
    condition = args[0]
    if condition:
        return StatementNode.ExecuteStoredProcedureNode(args[1])


internals = {
    "function": lambda args: StatementNode.FunctionDefinitionNode(args[0], args[1:len(args)]),
    "out": out,
    "readline": lambda args: input(args[0] if len(args) == 1 else ""),
    "length": lambda args: len(args[0]),
    "if": when,
    "exit": lambda args: exit(args[0] if len(args) == 1 else 0),
}

env = {
    "function": StatementNode.FunctionDefinitionNode("internal", []),
    "out": StatementNode.FunctionDefinitionNode("internal", []),
    "readline": StatementNode.FunctionDefinitionNode("internal", []),
    "length": StatementNode.FunctionDefinitionNode("internal", []),
    "if": StatementNode.FunctionDefinitionNode("internal", []),
    "exit": StatementNode.FunctionDefinitionNode("internal", []),
}
