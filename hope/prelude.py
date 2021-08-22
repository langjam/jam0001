from random import randint
from environment import Environment
from jltypes import *
from exceptions import JlTypeError


def jl_print(*args):
    if len(args) == 1:
        print(str(args[0]),  str(args[0].get_comment()))
    else:
        print(*map(str, args))


def jl_input():
    return JlString(input(),
                    JlComment("the user input"))


def jl_str(arg):
    return JlString(str(arg),
                    JlComment(f"{arg.get_comment().value} as a string"))


def jl_cmnt(arg):
    return JlComment(str(arg),
                     JlComment(f"{arg.get_comment().value} as a comment"))


def jl_num(arg):
    try:
        if isinstance(arg, JlString):
            return JlNumber(float(arg.value),
                            JlComment(f"{arg.get_comment().value} as a number"))
    except ValueError:
        pass

    return JlUnit()


def jl_list(*args):
    return JlList(list(args));


def jl_append(list, value):
    if not isinstance(list, JlList):
        raise JlTypeError("first argument must be a list")
    return list.value.append(value)


def jl_put(list, index, value):
    if not isinstance(list, JlList):
        raise JlTypeError("first argument must be a list")
    if not isinstance(index, JlNumber):
        raise JlTypeError("second argument must be a number")
    i = int(index.value)
    if i < len(list.value):
        list.value[i] = value
    return value


def jl_get(list, index):
    if not isinstance(list, JlList):
        raise JlTypeError("first argument must be a list")
    if not isinstance(index, JlNumber):
        raise JlTypeError("second argument must be a number")
    i = int(index.value)
    if i < len(list.value):
        return list.value[i]
    return JlUnit()


def jl_len(list):
    if not isinstance(list, JlList) and not isinstance(list, JlString):
        raise JlTypeError("first argument must be a list or string")
    return JlNumber(len(list.value),
                    JlComment(f"the length of {list.get_comment().value}"))


def jl_randint(min, max):
    if not isinstance(min, JlNumber):
        raise JlTypeError("first argument must be a number")
    if not isinstance(max, JlNumber):
        raise JlTypeError("second argument must be a number")
    return JlNumber(randint(min.value, max.value),
                    JlComment(f"a random integer between {min} and {max}"))


prelude = Environment()
prelude.bindings = {
    "print": JlPrimitive(jl_print, None, JlComment("the builtin print function")),
    "input": JlPrimitive(jl_input, 0, JlComment("the builtin input function")),
    "str": JlPrimitive(jl_str, 1, JlComment("the builtin str function")),
    "cmnt": JlPrimitive(jl_cmnt, 1, JlComment("the builtin cmnt function")),
    "num": JlPrimitive(jl_num, 1, JlComment("the builtin cmnt function")),
    "list": JlPrimitive(jl_list, None, JlComment("the builtin cmnt function")),
    "append": JlPrimitive(jl_append, 2, JlComment("the builtin append function")),
    "put": JlPrimitive(jl_put, 3, JlComment("the builtin put function")),
    "get": JlPrimitive(jl_get, 2, JlComment("the builtin get function")),
    "len": JlPrimitive(jl_len, 1, JlComment("the builtin len function")),
    "randint": JlPrimitive(jl_randint, 2, JlComment("the builtin randint function")),
}
