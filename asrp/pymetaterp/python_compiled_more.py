# Python 3, unlike other files which are Python 2
from pymetaterp.boot_compiled_more import *
from pymetaterp import boot_compiled_more

def token(s):
    while g.input.next() in ['\t', '\n', '\r', ' ']:
        pass
    g.input.position -= 1
    for char in s:
        if g.input.next() != char:
            return MatchError("Not exactly %s" % char)
    if char.isalpha():
        top = g.input.next()
        if top.isalnum() or top == '_':
            return MatchError("Prefix matched but didn't end.")
        g.input.position -= 1
    return s

def and_(children):
    saved = g.input.position
    outputs = []
    output_mode = None
    for child in children:
        output = child()
        if isinstance(output, MatchError):
            g.input.position = saved
            return MatchError("And match failed")
        if output_mode:
            if getattr(output, "name", None) == output_mode:
                outputs.extend(to_list(output.children))
        else:
            if getattr(output, "name", None) == "out":
                outputs = to_list(output.children)
                output_mode = "out"
            elif getattr(output, "name", None) == "rule_value":
                outputs = to_list(output.children)
                output_mode = "rule_value"
            else:
                outputs.extend(to_list(output))
    return "".join(outputs) if outputs and all(type(output) == str for output in outputs) and len(outputs[0]) == 1\
        else outputs

class Compiler(boot_compiled_more.Compiler):
    def eval_(self, root, indent):
        expr = root[0]
        # Maybe can just use python namespace?
        yield indented(indent, "self = g")
        yield indented(indent, f"output = {expr}")

    # Not a rule! Should rename this node to just 'value'?
    def rule_value(self, root, indent):
        # Not normally wrapped in Node, need to rethink!
        yield from self.eval_(root, indent)
        yield indented(indent, 'output = return Node("rule_value", output)')

    def predicate(self, root, indent):
        yield from self.eval_(root, indent)
        yield indented(indent, "if not output:")
        yield indented(indent+1, 'output = MatchError("Predicate evaluates to false")')
        yield indented(indent, "elif output == True:")
        yield indented(indent+1, "output = None")
        yield indented(indent, "else:")
        yield indented(indent+1, 'output = Node("predicate", [output])')

    def lookahead(self, root, indent):
        yield indented(indent, f"saved_{indent} = g.input.position # lookahead")
        yield from self.to_python(root[0], indent)
        yield indented(indent, f"g.input.position = saved_{indent}")

    def action(self, root, indent):
        expr = root[0]
        # Maybe can just use python namespace?
        yield indented(indent, "self = g")
        yield indented(indent, expr)
        yield indented(indent, "output = None")

    def bound(self, root, indent):
        name = root[1][0]
        type_ = root[1].name
        yield indented(indent, f"saved_{indent} = g.input.position # bound")
        yield from self.to_python(root[0], indent)
        if type_ == "inline":
            yield indented(indent, "if not isinstance(output, MatchError):")
            yield indented(indent+1, f"output = Node('{name}', output, (saved_{indent}+1, g.input.position+1))")
        else:
            # Could possibly use locals() instead of g.locals ?
            assert name != "output"
            yield indented(indent, f"{name} = output")
            yield indented(indent, f"output = []")

    def rule(self, root, indent):
        name = root[0][0]
        if name in ["anything", "letter", "digit"]:
            return
        #yield indented(indent, '@profile')
        yield indented(indent, f'def apply_{name}():')
        yield indented(indent+1, "if g.debug:")
        yield indented(indent+2, f'print(" "*g.nest, "{name}", g.input.source[g.input.position+1: g.input.position+10])')
        yield indented(indent+1, f"key = ('{name}', id(g.input.source), g.input.position, tuple(g.indentation))")
        yield indented(indent+1, "if key in g.memoizer:")
        yield indented(indent+2, "g.input.source, g.input.position = g.memoizer[key][1][:]")
        yield indented(indent+2, "if g.debug:")
        yield indented(indent+3, f'print(" "*g.nest, "{name}", "-m>", g.memoizer[key][0])')
        yield indented(indent+2, "return g.memoizer[key][0]")

        yield indented(indent+1, "g.nest += 1")
        yield indented(indent+1, f"saved_{indent} = g.input.position # rules")
        yield from self.to_python(root[-1], indent+1)
        yield indented(indent+1, "g.nest -= 1")
        yield indented(indent+1, "if g.debug:")
        yield indented(indent+2, f'print(" "*g.nest, "{name}", "->", output)')
        yield indented(indent+1, "if isinstance(output, MatchError):")
        yield indented(indent+2, "return output")
        if name == "escaped_char":
            yield indented(indent+1, r"""output = {"t": "\t", "n": "\n", "\\\\": "\\", "r": "\r"}.get(output, output)""")
        elif "!" in root[1]:
            yield indented(indent+1, f"""output = Node('{name}', output, (saved_{indent}+1, g.input.position+1))""")
        else:
            yield indented(indent+1, """if isinstance(output, list) and len(output) > 1:""")
            yield indented(indent+2, f"""output = Node('{name}', output, (saved_{indent}+1, g.input.position+1))""")
        yield indented(indent+1, 'g.memoizer[key] = (output, [g.input.source, g.input.position])')
        yield indented(indent+1, 'return output')

def apply_void():
    return

def reformat_atom(atom, trailers):
    if trailers:
        breakpoint()
    output = atom
    for trailer in trailers:
        pos = (output.pos[0], trailer.pos[1])
        if trailer.name == "arglist":
            output = Node("__call__", [output, trailer], pos=pos)
        elif trailer.name == "NAME":
            output = Node("__getattr__", [output, Node("NAME", trailer,
                                                       pos=trailer.pos)], pos=pos)
        elif trailer.name == "subscriptlist":
            output = Node("__getitem__", [output] + trailer, pos=pos)
        else:
            raise Exception("Unknown trailer %s" % trailer.name)
    return output


binary_ops = ((">=", "<=", "<>", "<", ">", "==", "!=",
               "in", "not in", "is not", "is"),
              ("|",), ("^",), ("&",), ("<<", ">>"), ("+", "-"),
              ("*", "/", "%", "//"), ("**",))
priority = {op:i for i, ops in enumerate(binary_ops) for op in ops}
expr_ops = binary_ops[1:]

def reformat_binary(start, oper_and_atoms):
    def parse(lhs, tokens, index=0):
        threshold = priority[tokens[index][0][0]]
        while index < len(tokens):
            op, rhs = tokens[index]
            assert(type(op) != str)
            op = op[0]
            if priority[op] < threshold:
                break
            index += 1
            while index < len(tokens) and\
                  priority[tokens[index][0][0]] > priority[op]:
                rhs, index = parse(rhs, tokens, index)
            lhs = Node("__binary__", [op, lhs, rhs], pos=(lhs.pos[0], rhs.pos[1]))
        return (lhs, index)
    if not oper_and_atoms:
        return start
    tokens = list(zip(oper_and_atoms[::2], oper_and_atoms[1::2]))
    lhs, index = start[0], 0
    while index < len(tokens):
        lhs, index = parse(lhs, tokens, index)
    return lhs

def any_token(input, binary=True):
    ops = binary_ops if binary else expr_ops
    old_input = g.input.position
    for tokens in ops:
        for token in tokens:
            if all(g.input.next() == char for char in token):
                return token
            g.input.position = old_input
    return False

def grammar_python(rules):
    return "\n".join(rules_to_python2(rules, Compiler))

def prewrite(tree):
    open("pytmp.py", "w").write(grammar_python(tree))

def match(tree, inp, debug=False, locals=None, cached=False, full=False):
    g.rules = {'anything': (apply_anything, ''), 'letter': (apply_letter, ''),
               'digit': (apply_digit, ''), 'void': (apply_void, ''),}
    g.indentation = [0]
    g.memoizer = {}
    g.locals = g.default_locals = {} if locals is None else dict(locals)
    g.nest = 0
    g.debug = debug
    if cached:
        if cached == "write":
            open("pytmp.py", "w").write(grammar_python(tree))
        import pytmp
        for key, value in globals().items():
            setattr(pytmp, key, value)
        g.input = Source(inp)
        result = pytmp.apply_grammar()
    else:
        open("pytmp.py", "w").write(grammar_python(tree))
        exec(grammar_python(tree), globals())
        g.input = Source(inp)
        result = apply_grammar()
    if full and not g.input.position == len(g.input.source) - 1:
        next_chars = g.input.source[g.input.position+1:g.input.position+101]
        return MatchError("Not all input consumed. Stopped at %s: %s" %\
                          (g.input.position + 1, next_chars))
    return result
