import time

from application_stack_utils import StatementNode


def out(args):
    for i in args:
        print(i, end=" ")

    print()


# This method is called "when" because it can´t be called if
def when(args):
    condition = args[0]
    if condition:
        return StatementNode.ExecuteStoredProcedureNode(args[1])


def _for(args):
    varname = args[0]
    tgetfrm = args[1]
    execute = args[2]
    return StatementNode.ForLoopExecutorNode(varname, tgetfrm, execute)


def _forever(args):
    return StatementNode.ForEverLoopExecutorNode(args[0])


def intrange(args):
    return list(range(args[0], args[1]))


def intrange_inclusive(args):
    return list(range(args[0], args[1] + 1))


def setlist(args):
    args[0][args[1]] = args[2]


internals = {
    "function": lambda args: StatementNode.FunctionDefinitionNode(args[0]),
    "class": lambda args: StatementNode.ClassDefinitionNode(args[0]),
    "out": out,
    "readline": lambda args: input(args[0] if len(args) == 1 else ""),
    "length": lambda args: len(args[0]),
    "if": when,
    "exit": lambda args: exit(args[0] if len(args) == 1 else 0),
    "for": _for,
    "forever": _forever,
    "list_add": lambda args: args[0].append(args[1]),
    "list_clear": lambda args: args[0].clear(),
    "list_extend": lambda args: args[0].extend(args[1]),
    "list_pop": lambda args: args[0].pop() if len(args) == 1 else args[0].pop(args[1]),
    "list_reverse": lambda args: args[0].reverse(),
    "list_sort": lambda args: args[0].sort(),
    "list_get": lambda args: args[0][args[1]],
    "list_set": setlist,
    "intrange": intrange,
    "intrange_inclusive": intrange_inclusive,
    "string_startswith": lambda args: args[0].startswith(args[1]),
    "string_endswith": lambda args: args[0].endswith(args[1]),
    "string_islower": lambda args: args[0].islower(),
    "string_isupper": lambda args: args[0].isupper(),
    "string_lower": lambda args: args[0].lower(),
    "string_upper": lambda args: args[0].upper(),
    "string_removeprefix": lambda args: args[0].removeprefix(args[1]),
    "string_removesuffix": lambda args: args[0].removesuffix(args[1]),
    "string_replace": lambda args: args[0].replace(args[1], args[2]),
    "string_split": lambda args: args[0].split(args[1]),
    "string_contains": lambda args: args[1] in args[0],
    "sleep": lambda args: time.sleep(args[0] / 1000)
}

env = {
    "function": StatementNode.FunctionDefinitionNode("internal"),
    "class": StatementNode.FunctionDefinitionNode("internal"),
    "out": StatementNode.FunctionDefinitionNode("internal"),
    "readline": StatementNode.FunctionDefinitionNode("internal"),
    "length": StatementNode.FunctionDefinitionNode("internal"),
    "if": StatementNode.FunctionDefinitionNode("internal"),
    "exit": StatementNode.FunctionDefinitionNode("internal"),
    "for": StatementNode.FunctionDefinitionNode("internal"),
    "forever": StatementNode.FunctionDefinitionNode("internal"),
    "list_add": StatementNode.FunctionDefinitionNode("internal"),
    "list_clear": StatementNode.FunctionDefinitionNode("internal"),
    "list_extend": StatementNode.FunctionDefinitionNode("internal"),
    "list_pop": StatementNode.FunctionDefinitionNode("internal"),
    "list_reverse": StatementNode.FunctionDefinitionNode("internal"),
    "list_sort": StatementNode.FunctionDefinitionNode("internal"),
    "list_get": StatementNode.FunctionDefinitionNode("internal"),
    "list_set": StatementNode.FunctionDefinitionNode("internal"),
    "intrange": StatementNode.FunctionDefinitionNode("internal"),
    "intrange_inclusive": StatementNode.FunctionDefinitionNode("internal"),
    "string_startswith": StatementNode.FunctionDefinitionNode("internal"),
    "string_endswith": StatementNode.FunctionDefinitionNode("internal"),
    "string_islower": StatementNode.FunctionDefinitionNode("internal"),
    "string_isupper": StatementNode.FunctionDefinitionNode("internal"),
    "string_lower": StatementNode.FunctionDefinitionNode("internal"),
    "string_upper": StatementNode.FunctionDefinitionNode("internal"),
    "string_removeprefix": StatementNode.FunctionDefinitionNode("internal"),
    "string_removesuffix": StatementNode.FunctionDefinitionNode("internal"),
    "string_replace": StatementNode.FunctionDefinitionNode("internal"),
    "string_split": StatementNode.FunctionDefinitionNode("internal"),
    "string_contains": StatementNode.FunctionDefinitionNode("internal"),
    "sleep": StatementNode.FunctionDefinitionNode("internal"),
}
