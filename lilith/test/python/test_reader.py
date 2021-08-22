"""Tests covering the reader."""

from lilith.parser import Apply, Args, Block, Symbol
from lilith.reader import Module, read_buffer
import pytest


@pytest.mark.parametrize(
    "example, expected",
    [
        (
            """!def[main, lang[lil]]\nprint["hello, world"]\n""",
            Module(
                Symbol("&buff"),
                [],
                {
                    Symbol("main"): Block(
                        Apply(Symbol("lang"), Args([Symbol("lil")], {})),
                        ['print["hello, world"]'],
                    )
                },
            ),
        )
    ],
)
def test_read(example, expected):
    got = read_buffer(example)
    assert got == expected
