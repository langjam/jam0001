from application_stack_utils import StatementNode


def function(args):
    return StatementNode.FunctionDefinitionNode(args[0], args[1:len(args)])


def out(args):
    for i in args:
        print(i, end=" ")

    print()


def readline(args):
    return input(args[0] if len(args) == 1 else "")


def length(args):
    return len(args[0])


def when(args):
    condition = args[0]
    if condition:
        return StatementNode.ExecuteStoredProcedureNode(args[1])


internals = {
    "function": function,
    "out": out,
    "readline": readline,
    "length": length,
    "if": when,
}

env = {
    "function": StatementNode.FunctionDefinitionNode("internal", []),
    "out": StatementNode.FunctionDefinitionNode("internal", []),
    "readline": StatementNode.FunctionDefinitionNode("internal", []),
    "length": StatementNode.FunctionDefinitionNode("internal", []),
    "if": StatementNode.FunctionDefinitionNode("internal", []),
}
