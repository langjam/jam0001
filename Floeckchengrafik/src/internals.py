from application_stack_utils import StatementNode


def function(args):
    print(args[0])
    return StatementNode.FunctionDefinitionNode(args[0], args[1:len(args)])


def out(args):
    for i in args:
        print(i, end=" ")

    print()


def readline(args):
    return input(args[0] if len(args) == 1 else "")


internals = {
    "function": function,
    "out": out,
    "readline": readline
}

env = {
    "function": StatementNode.FunctionDefinitionNode("internal", []),
    "out": StatementNode.FunctionDefinitionNode("internal", []),
    "readline": StatementNode.FunctionDefinitionNode("internal", [])
}
