"""
Pytest fixtures.
"""

from lilith.parser import Block, GRAMMAR, parser_with_transformer
import pytest


@pytest.fixture
def args_grammar():
    return parser_with_transformer(GRAMMAR, "args")


@pytest.fixture
def kwargs_grammar():
    return parser_with_transformer(GRAMMAR, "kwargs")


@pytest.fixture
def arguments_grammar():
    return parser_with_transformer(GRAMMAR, "arguments")


@pytest.fixture
def expr_grammar():
    return parser_with_transformer(GRAMMAR, "expr")


@pytest.fixture
def header_grammar():
    return parser_with_transformer(GRAMMAR, "header")
