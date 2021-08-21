from application_stack_utils import StatementNode


def function(args):
    return StatementNode.FunctionDefinitionNode(args[0], args[1:len(args)])


internals = {
    "function": function
}

env = {
    "function": StatementNode.FunctionDefinitionNode("internal", [])
}
