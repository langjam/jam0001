"""

"""

from lilith.interpreter import Bindings, eval, Runtime
from lilith.parser import Apply, Args, Symbol
from lilith.reader import Module
import pytest


@pytest.fixture
def runtime():
    return Runtime("test", None, {})


@pytest.mark.parametrize(
    "expr, expected",
    [
        (1, 1),
        ([1, 2], [1, 2]),
        ({"foo": "bar"}, {"foo": "bar"}),
    ],
)
def test_eval(runtime, expr, expected):
    assert (
        eval(
            runtime,
            Module("__repl__", [], dict()),
            Bindings("__root__", None),
            expr,
        )
        == expected
    )


def test_hello_world(capsys, runtime):
    assert (
        eval(
            runtime,
            Module("__repl__", [], {Symbol("print"): print}),
            Bindings("__root__", None),
            Apply(Symbol("print"), Args(["hello, world"], {})),
        )
        is None
    )
    captured = capsys.readouterr()
    assert captured.out == "hello, world\n"
