from application_stack_utils import StatementNode


def function(args):
    return StatementNode.FunctionDefinitionNode(args[0], args[1:len(args)])


def out(args):
    for i in args:
        print(i, end=" ")

    print()


def readin(args):
    input(args[0] if len(args) == 1 else "")


internals = {
    "function": function,
    "out": out,
    "readin": readin
}

env = {
    "function": StatementNode.FunctionDefinitionNode("internal", []),
    "out": StatementNode.FunctionDefinitionNode("internal", []),
    "readin": StatementNode.FunctionDefinitionNode("internal", [])
}
