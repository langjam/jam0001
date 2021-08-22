"""
A quick and dirty recursive interpreter for Lilith.
"""

import logging
import typing as t

from lilith.parser import Apply, Block, Symbol, Args
from lilith.reader import Module, Import


log = logging.getLogger(__name__)


class Runtime(t.NamedTuple):
    name: str
    prelude: Symbol
    modules: t.Dict[Symbol, Module]


class BindingNotFound(KeyError):
    def __init__(self, msg, context):
        super().__init__(msg)
        self.context = context


class Bindings(object):
    def __init__(self, name=None, bindings={}, parent=None):
        self._name = None
        self._parent = None
        self._bindings = bindings or {}

    def get(self, key):
        if key in self._bindings:
            return self._bindings.get(key)
        elif self._parent:
            try:
                return self._parent.get(key)
            except BindingNotFound as e:
                raise BindingNotFound(str(e), self)
        else:
            raise BindingNotFound(f"No binding key {key}", self)


def lookup(ctx, mod, locals, name):
    """Implement resolving a name against multiple namespaces."""

    # First locals
    err = None
    try:
        # Locals have already been evaluated
        return locals.get(name)
    except BindingNotFound as e:
        err = e

    # Note that module globals, and code in other modules IS DEFERRED. We have
    # to eval it before we can use it as a value. Eval IS RECURSIVE.

    # Then module scope
    if name in mod.defs:
        return eval(ctx, mod, Bindings(), mod.defs.get(name))

    # Then module imports
    # Note that we include the prelude as a fallback import here
    for i in mod.imports + [Import(ctx.prelude, {}, wild=True)]:
        im = ctx.modules.get(i.src)
        iname = i.names.get(name, name if i.wild else None)
        if iname in im.defs:
            return eval(ctx, im, Bindings(), im.defs.get(iname))

    # Finally try a global reference
    mod = Symbol(".".join(name.name.split(".")[:-1]))
    name = Symbol(name.name.split(".")[-1])
    if mod := ctx.modules.get(mod):
        if binding := mod.defs.get(name):
            return eval(ctx, mod, Bindings(), binding)

    raise err or KeyError


def eval(ctx: Runtime, mod: Module, locals: Bindings, expr):
    """Eval.

    In the context of a given runtime and module which must exist within the
    given runtime, evaluate the given expression recursively.

    """

    # Pointedly not assert that the module is ACTUALLY in the runtime,
    # We're just going to assume this for convenience.

    log.debug(f"eval[] {expr}")

    if isinstance(expr, Symbol):
        return lookup(ctx, mod, locals, expr)

    if isinstance(expr, Block):
        # Blocks are INCREDIBLY magical to the interpreter, as they have body
        # lines (understood to be body= as a kwarg) and any `lang[]` must be
        # evaluated before the body can be returned.

        body = expr.body
        expr = expr.app
        # Do eval of a synthetic expression capturing the requisite context.
        return eval(
            ctx,
            mod,
            # In a synthetic context with all the non-evaluatable
            # objects as locals
            Bindings(
                None,
                {
                    Symbol("runtime"): ctx,
                    Symbol("module"): mod,
                    Symbol("expr"): expr,
                    Symbol("body"): body,
                },
                locals,
            ),
            # Evaluate a synthetic expr using the locals to get kwargs
            # through.
            Apply(
                expr,
                Args(
                    [],
                    {
                        "runtime": Symbol("runtime"),
                        "module": Symbol("module"),
                        "expr": Symbol("expr"),
                        "body": Symbol("body"),
                    },
                ),
            ),
        )

    elif isinstance(expr, Apply):
        # Evaluate the call target
        fun = eval(ctx, mod, locals, expr.target)
        # Evaluate the parameters
        args = eval(ctx, mod, locals, expr.args.positionals)
        kwargs = eval(ctx, mod, locals, expr.args.kwargs)
        # Use Python's __call__ protocol
        log.debug(f"eval[]; app; {fun}[*{args or []}, **{kwargs or dict()}]")
        return fun(*args, **kwargs)

    elif isinstance(expr, list):
        return [eval(ctx, mod, locals, i) for i in expr]

    elif isinstance(expr, tuple):
        return tuple(eval(ctx, mod, locals, i) for i in expr)

    elif isinstance(expr, dict):
        return {
            eval(ctx, mod, locals, k): eval(ctx, mod, locals, v)
            for k, v in expr.items()
        }

    else:
        return expr
