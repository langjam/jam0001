"""tests covering the Lilith parser."""

from lilith.parser import Apply, Args, Block, GRAMMAR, parse_buffer, parser_with_transformer, Symbol
import pytest


@pytest.mark.parametrize(
    "example, expected",
    [
        ("1", [1]),
        ("1, 2", [1, 2]),
        ("1, 2, 3", [1, 2, 3]),
    ],
)
def test_parse_args(args_grammar, example, expected):
    assert args_grammar.parse(example) == expected


@pytest.mark.parametrize(
    "example, expected",
    [
        ("foo: bar", {Symbol("foo"): Symbol("bar")}),
        (
            "foo: bar, baz: qux",
            {Symbol("foo"): Symbol("bar"), Symbol("baz"): Symbol("qux")},
        ),
    ],
)
def test_parse_kwargs(kwargs_grammar, example, expected):
    assert kwargs_grammar.parse(example) == expected


@pytest.mark.parametrize(
    "example, expected",
    [
        ("1", ([1], {})),
        ("1, 2", ([1, 2], {})),
        ("1, 2, 3", ([1, 2, 3], {})),
        ("foo: bar", ([], {Symbol("foo"): Symbol("bar")})),
        (
            "foo: bar, baz: qux",
            ([], {Symbol("foo"): Symbol("bar"), Symbol("baz"): Symbol("qux")}),
        ),
        (
            "1; foo: bar, baz: qux",
            ([1], {Symbol("foo"): Symbol("bar"), Symbol("baz"): Symbol("qux")}),
        ),
    ],
)
def test_parse_arguments(arguments_grammar, example, expected):
    assert arguments_grammar.parse(example) == expected


@pytest.mark.parametrize(
    "example, expected",
    [
        ("1", 1),
        ("[1, 2, 3]", [1, 2, 3]),
        ('{"a": 1}', {"a": 1}),
        ("true", True),
        ("false", False),
        ("nil", None),
    ],
)
def test_parse_expr(expr_grammar, example, expected):
    assert expr_grammar.parse(example) == expected


@pytest.mark.parametrize(
    "example, expected",
    [
        ("!def[syntax]", Block(Apply(Symbol("def"), Args([Symbol("syntax")], {})), [])),
        (
            "!frag[lang: md]",
            Block(Apply(Symbol("frag"), Args([], {Symbol("lang"): Symbol("md")})), []),
        ),
        (
            "!frag[foo; lang: md]",
            Block(
                Apply(
                    Symbol("frag"),
                    Args([Symbol("foo")], {Symbol("lang"): Symbol("md")}),
                ),
                [],
            ),
        ),
        ("!int.add[1, 2]", Block(Apply(Symbol("int.add"), Args([1, 2], {})), [])),
    ],
)
def test_parse_header(header_grammar, example, expected):
    assert header_grammar.parse(example) == expected


@pytest.mark.parametrize(
    "example, expected",
    [
        (
            "!frag[lang: md]",
            [
                Block(
                    Apply(Symbol("frag"), Args([], {Symbol("lang"): Symbol("md")})), []
                )
            ],
        ),
        (
            """!frag[lang: md]\nHello, world!\n\n""",
            [
                Block(
                    Apply(Symbol("frag"), Args([], {Symbol("lang"): Symbol("md")})),
                    ["Hello, world!", ""],
                )
            ],
        ),
        (
            """!frag[lang: md]\nHello, world!\n\n!def[bar]""",
            [
                Block(
                    Apply(Symbol("frag"), Args([], {Symbol("lang"): Symbol("md")})),
                    ["Hello, world!", ""],
                ),
                Block(Apply(Symbol("def"), Args([Symbol("bar")], {})), []),
            ],
        ),
    ],
)
def test_block_parser(example, expected):
    assert parse_buffer(example) == expected
