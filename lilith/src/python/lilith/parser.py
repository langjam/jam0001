"""
Variously poor parsing for Lilith.
"""

import typing as t
from importlib.resources import read_text

from lark import Lark, v_args, Transformer

GRAMMAR = read_text("lilith", "grammar.lark")


# !foo[bar]
# !def[name]
# !frag[lang: yaml]
# !end
# all this following tex
class Symbol(t.NamedTuple):
    name: str


class Args(t.NamedTuple):
    positionals: object = []
    kwargs: object = {}


class Apply(t.NamedTuple):
    target: object
    args: Args


class Block(t.NamedTuple):
    app: Apply
    body_lines: list

    @property
    def tag(self):
        return self.app.tag

    @property
    def args(self):
        return self.app.args

    @property
    def body(self):
        return "\n".join(self.body_lines)


id = lambda x: x


class TreeToTuples(Transformer):
    @v_args(inline=True)
    def string(self, s):
        return s[1:-1].replace('\\"', '"')

    nil = lambda self, _: None
    true = lambda self, _: True
    false = lambda self, _: False
    int = v_args(inline=True)(lambda self, x: int(x))
    float = v_args(inline=True)(lambda self, x: float(x))
    number = v_args(inline=True)(lambda self, x: x)

    list = list
    dict = dict

    def word(self, args):
        """args: ['a'] ['a' ['b', 'c', 'd']]"""
        return Symbol(".".join(a.value for a in args))

    def atom(self, args):
        return args[0]

    def expr(self, args):
        return args[0]

    def application(self, args):
        tag = args[0]
        args = args[1] if len(args) > 1 else Args()
        return Apply(tag, args)

    def args(self, args):
        _args = [args[0]]
        if len(args) == 2:
            _args = _args + args[1]
        return _args

    def pair(self, args):
        return (args[0], args[1])

    def kwargs(self, args):
        d = {}
        key, val = args[0]
        if len(args) == 2:
            d.update(args[1])
        d[key] = val
        return d

    def a_args_kwargs(self, args):
        return Args(args[0], args[1])

    def a_args(self, args):
        return Args(args[0], {})

    def a_kwargs(self, args):
        return Args([], args[0])

    def arguments(self, args):
        return args[0]

    def header(self, args):
        return Block(args[0], [])


def parser_with_transformer(grammar, start="header"):
    return Lark(grammar, start=start, parser="lalr", transformer=TreeToTuples())


def parse_expr(buff: str):
    return parser_with_transformer(GRAMMAR, "expr").parse(buff)


def parse_buffer(buff: str, name: str = "&buff") -> t.List[object]:
    header_parser = parser_with_transformer(GRAMMAR, "header")

    def _parse():
        block = None
        for line in buff.splitlines():
            if line.startswith("!"):
                if block:
                    yield block
                block = header_parser.parse(line)
            elif block:
                block.body_lines.append(line)
            else:
                raise SyntaxError("Buffers must start with a ![] block")
        if block:
            yield block

    return list(_parse())
