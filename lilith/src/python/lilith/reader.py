"""
Lilith's reader takes parsed blocks and applies languages, building a module structure.
"""

import logging
import typing as t
from warnings import warn

from lilith.parser import Args, Block, parse_buffer, Symbol


log = logging.getLogger(__name__)


class Import(t.NamedTuple):
    src: Symbol
    names: t.Dict[Symbol, Symbol]
    wild: bool = False


class Module(t.NamedTuple):
    name: Symbol
    imports: t.List[Import]
    defs: t.Dict[str, Block]


def read_buffer(buffer: str, name: str = "&buff") -> Module:
    """Read a module out of a string [or file]"""

    m = Module(Symbol(name), [], {})
    for block in parse_buffer(buffer, name):
        if block.app.target == Symbol("def"):
            if len(block.args.positionals) == 2:
                def_name, expr = block.args.positionals
                m.defs[def_name] = Block(expr, block.body_lines)
            else:
                raise SyntaxError("!def[<name>, <expr>; <kwargs>] <body>")

            if block.args.kwargs:
                warn("!def[<kwargs>] are ignored")

        elif block.app.target == Symbol("import"):
            # FIXME (arrdem 2021-08-21):
            #   This doesn't simplify imports as it goes.
            #   Multiple imports from the same source will wind up with multiple importlist entries.
            iname = block.args.positionals[0]
            wild = block.args.kwargs.get(Symbol("wild"), False)
            rename = block.args.kwargs.get(Symbol("as"), {})
            imports = (
                block.args.positionals[1] if len(block.args.positionals) == 2 else []
            )
            m.imports.append(
                Import(iname, {rename.get(i, i): i for i in imports}, wild)
            )

        else:
            raise SyntaxError(f"Unsupported block !{block.tag}[..]")

    return m


def read_file(path: str):
    """Read a module out of a file."""

    with open(path) as fp:
        return read_buffer(fp.read(), path)
