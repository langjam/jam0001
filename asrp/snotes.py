import sys
from pymetaterp.boot_compiled_more import match, Node, simple_wrap_tree
from pymetaterp.python_compiled_more import match as py_match, prewrite
from pymetaterp import boot_tree, boot_grammar
import inspect

grammar = open("snotes.grammar").read()

t1 = list(simple_wrap_tree(boot_tree.tree))
t2 = match(t1, boot_grammar.bootstrap + boot_grammar.extra + boot_grammar.diff, full=True)
lang_tree = match(t2, grammar + boot_grammar.extra, full=True)

class Transformer:
    def transform(self, node):
        if isinstance(node, Node):
            if hasattr(self, node.name):
                return getattr(self, node.name)(node)
            else:
                node.children = [self.transform(child) for child in node]
                return node
        return node

    def infix(self, node):
        res = node[0]
        for i in range(0, len(node) - 1, 2):
            if node[i+2].name == "func_call" and node[i+1][0] == ".":
                res = Node("send", [res, Node("STRING", node[i+2][0][0]), node[i+2][1]])
            elif node[i+1][0] == ".":
                res = Node("attrget", [res, Node("STRING", node[i+2][0])])
            else:
                res = Node(node[i+1][0], [res, node[i+2]])
            res.pos = node[i+1].pos
            res.children = [self.transform(child) for child in res.children]
        return res

    def func_call(self, node):
        name, params = node
        res = Node(name[0], params, pos=node.pos)
        res.children = [self.transform(child) for child in res.children]
        return res

# How many base objects do we need?
# 3 base objects: World, Node, Obj, Block ..., Frame, Scope

class Obj:
    def __init__(self, name=None, attr=None):
        self.attr = {} if attr is None else attr
        self.name = name

    def lang_getattr(self, key):
        return self.attr[str(key)]

    def lang_setattr(self, key, value):
        self.attr[str(key)] = value

    def add_comment(self, value):
        if "comment" not in self.attr:
            self.attr["comment"] = value
        else:
            self.attr["comment"] += ", "
            self.attr["comment"] += value

    def __str__(self):
        comment = self.attr.get('comment')
        if comment:
            return f"{self.name} obj /* {comment} */"
        else:
            return f"{self.name} obj"

    def __repr__(self):
        return f"Obj({self.name})"

class Str(Obj):
    def __init__(self, s):
        super().__init__("Str")
        if not isinstance(s, str):
            breakpoint()
        self.s = s
    def __str__(self):
        return self.s
    def __repr__(self):
        return f"Str({self.s})"
    def call(self):
        g.data_stack.append(self)

class Comm(Str):
    def __repr__(self):
        return f"Comm({self.s})"
    def call(self):
        if cur_scope().get('show_comment') is not None and\
           cur_scope().get('show_comment') == cur_scope().get('True'):
            print(f"/* {self.s} */")
        g.data_stack[-1].add_comment(self.s)

class Num(Obj):
    def __init__(self, n):
        super().__init__("Num")
        self.n = n
    def __repr__(self):
        return f"Num({self.n})"
    def call(self):
        g.data_stack.append(self)

class Get:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return f"Get({self.name})"
    def call(self):
        value, scope = cur_scope()[self.name]
        g.data_stack.append(value)

class BoundMethod:
    def __init__(self, self_, method):
        self.self_ = self_
        self.method = method

    def eval(self):
        if hasattr(self.method, "eval"):
            g.data_stack.append(self.self_)
            return self.method.eval()
        else:
            args = [g.data_stack.pop() for arg in inspect.signature(self.method).parameters if arg != "self"]
            if self.self_ == glob["W"][0]:
                return self.method(*args)
            else:
                return self.method(self.self_, *args)

class Send:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return f"Send({self.name})"
    def call(self):
        obj = g.data_stack.pop()
        return call_obj(BoundMethod(obj, obj.lang_getattr(self.name)))

def Thunk_eval(self):
    return self.eval()

class Thunk(Obj):
    def __init__(self, body, params, parent):
        super().__init__("Block")
        self.attr = glob["Block"][0].attr
        self.body = body
        self.params = [p[0] for p in params]
        self.parent = parent
    def call(self):
        g.data_stack.append(self)
    def eval(self):
        parent_scope = self.lex_parent()[1].scope
        # TODO: fix this
        g.call_stack.append(Frame(self, parent_scope))
        for name in reversed(self.params):
            cur_scope()[name] = g.data_stack.pop()

    def lex_parent(self):
        for i, frame in enumerate(reversed(g.call_stack)):
            if self.parent == frame.block:
                return len(g.call_stack) - i - 1, frame
        else:
            raise Exception("Lexical parent not found")

    def __repr__(self):
        return f"Thunk({', '.join(self.params)})"

flat_skip = ["parameters", "body", "exprs"]

def deep_flatten(l):
    for el in l:
        if isinstance(el, list):
            yield from deep_flatten(el)
        elif el is not None and el not in flat_skip:
            yield el

def ldeep_flatten(l):
    return list(deep_flatten(l))

def lreversed(l):
    return list(reversed(list(l)))

escaped = {repr(x)[1:-1]:x for x in "\t\n\r\\"} #\'\"

class Flattener:
    def flatten(self, node):
        if isinstance(node, Node):
            if hasattr(self, node.name):
                return getattr(self, node.name)(node)
            else:
                res = lreversed(self.flatten(child) for child in node)
                return res + [node.name] if node.name not in flat_skip else res
        return node

    def NAME(self, node):
        return Get(node[0])

    def STRING(self, node):
        s = node[0]
        for esc, raw in escaped.items():
            s = s.replace(esc, raw)
        return Str(s)

    def COMMENT(self, node):
        s = node[0]
        for esc, raw in escaped.items():
            s = s.replace(esc, raw)
        return Comm(s)

    def NUMBER(self, node):
        return Num(int(node[0]))

    def suite(self, node):
        return [self.flatten(child) for child in node]

    def send(self, node):
        receiver, message, params = node
        return [self.flatten(params), self.flatten(receiver), Send(message[0])]

    def assign(self, node):
        if node[0].name == "path":
            return [self.flatten(node[1]), Str(node[0][1][0]), self.flatten(node[0][0]), "attrset"]
        else:
            assert(node[0].name == "NAME")
            return [self.flatten(node[1]), Str(node[0][0]), "assign"]

    def block(self, node):
        params, body = node
        res = Thunk(None, params, parent=self.parent)
        old_parent = self.parent
        self.parent = res
        res.body = ldeep_flatten(self.flatten(body))
        self.parent = old_parent
        return res

Flattener.expr = Flattener.suite

def parse(filename, debug=False):
    return parse_source(open(filename).read(), debug=debug)

def parse_source(source, debug=False):
    res = py_match(lang_tree, source, debug=debug, full=True)
    res2 = Transformer().transform(res)
    flat = Flattener()
    res3 = Thunk(None, [], parent=None)
    flat.parent = res3
    res3.body = ldeep_flatten(flat.flatten(res2))
    return res2, res3

def func_call(func, *args):
    if hasattr(func, "__call__"):
        return func(*args)
    else:
        return func.call(*args)

class Scope:
    def __init__(self, parent):
        self.parent = parent
        self.values = {'__parent_env__': parent}

    def call(self, name, *args):
        func = self.get(name)
        if func:
            return func_call(func, *args)
        else:
            getattr(self, name)(*args)

    def get(self, key):
        if key in self.values:
            return self.values[key]
        if self.parent is None:
            return None
        return self.parent.get(key)

    def __getitem__(self, key):
        if key in self.values:
            return self.values[key], self
        if self.parent is None:
            raise KeyError(f"Key {key} not found")
        assert(self.parent != self)
        return self.parent[key]

    def __setitem__(self, key, value):
        self.values[key] = value

class Frame:
    def __init__(self, block, parent):
        self.block = block
        self.source = block.body
        self.pos = 0
        self.scope = Scope(parent)

    def current(self):
        return self.source[self.pos]

    def ended(self):
        return self.pos >= len(self.source)

    def __repr__(self):
        return f"Frame {self.pos} / {len(self.source)}"

# Could alternatively reify scope?
# assign = [ key value | W.locals().attrset(key value) ]
def assign(name, value):
    cur_scope()[str(name)] = value

def attrset(obj, key, value):
    if not isinstance(obj, Obj):
        setattr(obj, key, value)
    else:
        obj.lang_setattr(key, value)

def attrget(obj, key):
    return obj.lang_getattr(key)

def return_():
    dest_block = g.call_stack[-1].block.parent
    while g.call_stack[-1].block != dest_block:
        g.call_stack.pop()

def print_(s):
    print(s, end="")

def discard():
    g.data_stack.pop()

glob = Scope(None)
glob['attrset'] = attrset
glob['attrget'] = attrget
glob['assign'] = assign
glob['return'] = return_
glob['print'] = print_
glob['discard'] = discard
glob['Obj'] = Obj("Obj")
glob['W'] = Obj("World", glob.values)

def Obj_copy(self, name):
    return Obj(name, self.attr.copy())

glob['Obj'][0].lang_setattr("copy", Obj_copy)

def void_obj():
    return Obj_copy(glob["Void"][0], "Void")

glob['Void'] = Obj_copy(glob['Obj'][0], "Void")
glob['void_obj'] = void_obj
glob['Block'] = Obj_copy(glob['Obj'][0], "Block")
glob['Block'][0].lang_setattr("eval", Thunk_eval)

def bp():
    breakpoint()

glob['bp'] = bp
glob['breakpoint'] = bp

class Global:
    pass

g = Global()
g.data_stack = []
g.call_stack = []
g.source = []

def cur_scope():
    return g.call_stack[-1].scope

def call_obj(obj, scope=None):
    if hasattr(obj, "eval"):
        return obj.eval() #scope if scope is not None else cur_scope()
    else:
        args = [g.data_stack.pop() for arg in inspect.signature(obj).parameters if arg != "self"]
        return obj(*args)

# func or constant (just add to stack) or Send or Get
def call_frame(frame):
    if isinstance(frame, str):
        value, scope = cur_scope()[frame]
        res = call_obj(value, scope)
    else:
        res = frame.call()
    if res is not None:
        g.data_stack.append(res)

def run_flattened(flat_root):
    g.call_stack.append(Frame(flat_root, glob))
    while g.call_stack:
        frame = g.call_stack[-1].current()
        g.call_stack[-1].pos += 1
        call_frame(frame)
        while g.call_stack and g.call_stack[-1].ended():
            if len(g.call_stack) == 1:
                return
            g.call_stack.pop()

if __name__ == '__main__':
    filenames = sys.argv[1:]
    source = "\n\n".join(open(filename).read() for filename in filenames)
    root, flat_root = parse_source(source)
    run_flattened(flat_root)
