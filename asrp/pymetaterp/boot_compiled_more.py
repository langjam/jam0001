# Python 3, unlike other files which are Python 2
def simple_wrap_tree(root):
    if type(root) != list:
        return root
    return Node(root[0], list(map(simple_wrap_tree, root[1:])))

class MatchError(Exception):
    pass

inf = float("inf")

class Glob(object):
    pass

g = Glob()
g.debug = False
g.count = 0

class Source():
    def __init__(self, source):
        self.source = source
        self.position = -1
    def next(self):
        self.position += 1
        try:
            return self.source[self.position]
        except IndexError:
            return MatchError("EOF")

class Node(object):
    def __init__(self, name, children, pos=(None, None)):
        self.name = name
        self.children = children
        self.pos = pos
    def __getitem__(self, index):
        if type(self.children) == list:
            return self.children[index]
        else:
            return [self.children][index]
    def __len__(self):
        return len(self.children) if isinstance(self.children, list) else 1 if self.children else 0
    def __bool__(self):
        # Python 3 checks __len__ otherwise
        return True
    def __repr__(self):
        return "%s(%s)" % (self.name, self.children)
    def pprint(self, indent=0):
        print(" "*indent + self.name)
        children = [self.children] if not isinstance(self.children, list) else self.children
        for child in children:
            if not hasattr(child, "pprint"):
                print(" "*(indent + 1), type(child).__name__, repr(child))
            else:
                child.pprint(indent + 2)

def to_list(value):
    return value if isinstance(value, list) else\
           [] if value is None else\
           [value]

def exactly(char):
    ichar = g.input.next()
    return ichar if isinstance(ichar, MatchError) or char == ichar\
        else MatchError("Not exactly %s" % char)

def between(start, end):
    ichar = g.input.next()
    return ichar if isinstance(ichar, MatchError) or start <= ichar <= end\
        else MatchError("Not between %s and %s" % (start, end))

def token(s):
    while g.input.next() in ['\t', '\n', '\r', ' ']:
        pass
    g.input.position -= 1
    for char in s:
        if g.input.next() != char:
            return MatchError("Not exactly %s" % char)
    return s

def indented(indent, s):
    return indent * 2 * " " + s

class Compiler:
    def __init__(self):
        pass

    def to_python(self, root, indent=0):
        #yield getattr(root, "name", "")
        if type(root) == str:
            yield indented(indent, root)
        else:
            name = root.name + "_" if root.name in ["and", "or"] else root.name
            if not hasattr(self, name):
                print(name)
                breakpoint()
            yield from getattr(self, name)(root, indent)

    def and_(self, root, indent):
        output_mode = "output" if any(getattr(child, "name", None) == "output" for child in root) else\
            "rule_value" if any(getattr(child, "name", None) == "rule_value" for child in root)\
            else None
        yield indented(indent, f"saved_{indent} = g.input.position # and")
        yield indented(indent, f"outputs_{indent} = []")
        yield indented(indent, "for _ in [0]: # and")
        for child in root:
            yield from self.to_python(child, indent + 1)
            yield indented(indent+1, "if isinstance(output, MatchError):")
            yield indented(indent+2, f"g.input.position = saved_{indent}")
            yield indented(indent+2, f'output = outputs_{indent} = MatchError("And match failed")')
            yield indented(indent+2, 'break')
            if output_mode:
                if child.name == output_mode:
                    yield indented(indent+1, f"outputs_{indent}.extend(to_list(output))")
            else:
                yield indented(indent+1, f"outputs_{indent}.extend(to_list(output))")
        yield indented(indent, f"if not isinstance(outputs_{indent}, MatchError):")
        if output_mode:
            yield indented(indent+1, f"if len(outputs_{indent}) == 1:")
            yield indented(indent+2, f"outputs_{indent} = outputs_{indent}[0]")

        yield indented(indent+1, f'''output = "".join(outputs_{indent}) if outputs_{indent} and type(outputs_{indent}) == list and all(type(output) == str for output in outputs_{indent}) and len(outputs_{indent}[0]) == 1 else outputs_{indent}''')

    def or_(self, root, indent):
        yield indented(indent, f"saved_{indent} = g.input.position # or")
        yield indented(indent, "for _ in [0]: # or")
        for child in root:
            yield indented(indent+1, f"g.input.position = saved_{indent}")
            yield from self.to_python(child, indent + 1)
            yield indented(indent+1, "if not isinstance(output, MatchError):")
            yield indented(indent+2, "break")
        yield indented(indent, "else:")
        yield indented(indent+1, f"g.input.position = saved_{indent}")
        yield indented(indent+1, 'output = MatchError("No OR child matches")')

    def apply(self, root, indent):
        yield indented(indent, f"output = apply_{root[0]}()")

    def token(self, root, indent):
        yield indented(indent, f"output = token({repr(root[0])})")

    def output(self, root, indent):
        assert(len(root) <= 1)
        if len(root):
            yield from self.to_python(root[0], indent)
        else:
            yield indented(indent, "output = None")

    def quantified(self, root, indent):
        quantifier = root[1][0]
        lower, upper = {"*": (0, inf), "+": (1, inf), "?": (0, 1)}[quantifier]
        yield indented(indent, f"outputs_{indent} = []")
        yield indented(indent, f"count_{indent} = 0")
        yield indented(indent, f"start_saved_{indent} = g.input.position")
        yield indented(indent, f"while count_{indent} < {upper}:")
        yield indented(indent+1, f"saved_{indent} = g.input.position # quantified")
        yield from self.to_python(root[0], indent + 1)
        yield indented(indent+1, "if isinstance(output, MatchError):")
        yield indented(indent+2, f"if count_{indent} < {lower}:")
        yield indented(indent+3, f"g.input.position = start_saved_{indent}")
        yield indented(indent+3, f'output = MatchError(f"Quantified undermatch {{ count_{indent} }} < {lower}")')
        yield indented(indent+3, f'break')
        yield indented(indent+2, "else:")
        yield indented(indent+3, f"g.input.position = saved_{indent}")
        yield indented(indent+3, f"output = outputs_{indent}")
        yield indented(indent+3, f"break")
        yield indented(indent+1, f"outputs_{indent}.extend(to_list(output))")
        yield indented(indent+1, f"count_{indent} += 1")
        yield indented(indent, f"else:")
        yield indented(indent+1, f"output = outputs_{indent}")

    def negation(self, root, indent):
        yield indented(indent, f"saved_{indent} = g.input.position # negation")
        yield from self.to_python(root[0], indent)
        yield indented(indent, f"g.input.position = saved_{indent}")
        yield indented(indent, 'output = None if isinstance(output, MatchError) else MatchError("Negation_is_true")')

    def exactly(self, root, indent):
        char = root[0]
        yield indented(indent, "ichar = g.input.next()")
        yield indented(indent, f'''output = ichar if isinstance(ichar, MatchError) or ichar == {repr(char)} else MatchError("Not exactly %s" % {repr(char)})''')

    def bound(self, root, indent):
        name = root[1][0]
        yield indented(indent, f"saved_{indent} = g.input.position # bound")
        yield from self.to_python(root[0], indent)
        yield indented(indent, "if not isinstance(output, MatchError):")
        yield indented(indent+1, f"output = Node('{name}', output, (saved_{indent}+1, g.input.position+1))")

    def rule(self, root, indent):
        name = root[0][0]
        if name in ["anything", "letter", "digit"]:
            return
        yield indented(indent, f'def apply_{name}():')
        yield indented(indent+1, f"saved_{indent} = g.input.position # rules")
        yield from self.to_python(root[-1], indent+1)
        yield indented(indent+1, "if isinstance(output, MatchError):")
        yield indented(indent+2, "return output")
        if name == "escaped_char":
            yield indented(indent+1, r"""return({"t": "\t", "n": "\n", "\\\\": "\\", "r": "\r"}.get(output, output))""")
        elif name == "balanced":
            yield indented(indent+1, r"return output")
        elif "!" in root[1]:
            yield indented(indent+1, f"""return Node('{name}', output, (saved_{indent}+1, g.input.position+1))""")
        else:
            yield indented(indent+1, """if isinstance(output, list) and len(output) > 1:""")
            yield indented(indent+2, f"""return Node('{name}', output, (saved_{indent}+1, g.input.position+1))""")
            yield indented(indent+1, "return output")

def rules_to_python2(rules, compiler=Compiler):
    compiler = compiler()
    for rule in rules:
        yield from compiler.to_python(rule)
        yield ""

def and_(children):
    saved = g.input.position
    outputs = []
    output_mode = False
    for child in children:
        output = child()
        if isinstance(output, MatchError):
            g.input.position = saved
            return MatchError("And match failed")
        if output_mode:
            if getattr(output, "name", None) == "out":
                outputs.extend(to_list(output.children))
        else:
            if getattr(output, "name", None) == "out":
                # There was an asymmetry between the first
                # child nad subsequent ones!
                # outputs = output.children
                outputs = to_list(output.children)
                output_mode = True
            else:
                outputs.extend(to_list(output))
    if output_mode and len(outputs) == 1:
        outputs = outputs[0]
    return "".join(outputs) if outputs and type(outputs) == list and all(type(output) == str for output in outputs) and len(outputs[0]) == 1\
        else outputs

def apply_anything():
    char = g.input.next()
    return MatchError("End_of_file") if char is None else char

def apply_letter():
    saved = g.input.position
    output = between("a", "z")
    if isinstance(output, MatchError):
        g.input.position = saved
        # Should return custom MatchError message
        return between("A", "Z")
    else:
        return output

def apply_digit():
    return(between("0", "9"))

def gen_apply(rule):
    name = rule[0][0]
    if name in ["anything", "letter", "digit"]:
        return ""

    out = """
def apply_{name}():
    saved = g.input.position
    output = {func_body}
    if isinstance(output, MatchError):
        return output
"""
    if name == "escaped_char":
        return out.format(func_body = g.output_rules[name], name=name) +\
            r"""    return({"t": "\t", "n": "\n", "\\\\": "\\", "r": "\r"}.get(output, output))""" + "\n"
    elif "!" in rule[1]:
        out += """    return Node('{name}', output, (saved+1, g.input.position+1))
"""
    else:
        out += """    if isinstance(output, list) and len(output) > 1:
        return Node('{name}', output, (saved+1, g.input.position+1))
    return output
    """
    return out.format(func_body=g.output_rules[name], name=name)

# Want mostly one function to avoid superfluous function calls?
# C has an inline primitive
# Feedback vertex set?
# Maybe manually declare in this case, such as "expr"
def all_calls(roots):
    return {root[0][0]: set(calls(root)) for root in roots}

def calls(root):
    if not isinstance(root, str):
        if root.name == "apply":
            yield root[0]
        else:
            for child in root:
                for call in calls(child):
                    yield call

def gen_from_tree():
    import boot_tree
    return "\n".join(rules_to_python2(list(simple_wrap_tree(boot_tree.tree))))

def grammar_python(rules):
    return "\n".join(rules_to_python2(rules))

def match(tree, inp, full=False):
    exec(grammar_python(tree), globals())
    g.input = Source(inp)
    result = list(apply_grammar())
    if full and not g.input.position == len(g.input.source) - 1:
        next_chars = g.input.source[g.input.position+1:g.input.position+101]
        return MatchError("Not all input consumed. Stopped at %s: %s" %\
                          (g.input.position + 1, next_chars))
    return result

def exec_(src, inp):
    exec(src, globals())
    g.input = Source(inp)
    return list(apply_grammar())
