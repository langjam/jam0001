from dataclasses import dataclass
from lark import ast_utils, visitors, Token
from typing import List
from jltypes import *

@dataclass
class SourceLocation:
    filename: str
    line: int
    column: int
    end_line: int
    end_column: int

    @classmethod
    def from_tree(self, tree, filename, line_offset=0):
        return SourceLocation(
            filename,
            tree.line + line_offset,
            tree.column,
            tree.end_line + line_offset,
            tree.end_column)


@dataclass
class Expr:
    location: SourceLocation


@dataclass
class CommentedExpr(Expr):
    expr: Expr
    comment: Expr

    def accept(self, visitor):
        return visitor.visit_commented_expr(self)


@dataclass
class BinExpr(Expr):
    lhs: Expr
    op: Token
    rhs: Expr

    def accept(self, visitor):
        return visitor.visit_bin_expr(self)

@dataclass
class UnaryExpr(Expr):
    op: Token
    expr: Expr

    def accept(self, visitor):
        return visitor.visit_unary_expr(self)

@dataclass
class Literal(Expr):
    value: object # TODO: JlObject
    def accept(self, visitor):
        return visitor.visit_literal(self)


@dataclass
class Name(Expr):
    name: str
    binding_depth: int = None

    def accept(self, visitor):
        return visitor.visit_name(self)


@dataclass
class Assignment(Expr):
    name: Name
    expr: Expr

    def accept(self, visitor):
        return visitor.visit_assignment(self)


@dataclass
class Declaration(Expr):
    name: Name
    expr: Expr

    def accept(self, visitor):
        return visitor.visit_declaration(self)


@dataclass
class IfExpr(Expr):
    cond: Expr
    then_body: Expr
    else_body: Expr = None

    def accept(self, visitor):
        return visitor.visit_if_expr(self)


@dataclass
class WhileExpr(Expr):
    cond: Expr
    body: Expr


    def accept(self, visitor):
        return visitor.visit_while_expr(self)


@dataclass
class CallExpr(Expr):
    f: Expr
    args: List[Expr]

    def accept(self, visitor):
        return visitor.visit_call(self)


@dataclass
class FnExpr(Expr):
    params: List[Name]
    body: Expr

    def accept(self, visitor):
        return visitor.visit_fn_expr(self)


@dataclass
class ExplainExpr(Expr):
    expr: Expr

    def accept(self, visitor):
        return visitor.visit_explain_expr(self)


@dataclass
class Block(Expr):
    exprs: List[Expr]

    def accept(self, visitor):
        return visitor.visit_block(self)


@dataclass
class Program(Expr):
    exprs: List[Expr]

    def accept(self, visitor):
        return visitor.visit_program(self)


class AstVisitor:
    def visit(self, ast):
        return ast.accept(self)


class AstPrinter(AstVisitor):
    def __init__(self):
        self.indent = 0

    def print_indent(self):
        print(" |" * self.indent, end='')

    def visit_program(self, block):
        self.print_indent()
        print('Program')
        self.indent += 1
        for e in block.exprs:
            self.visit(e)
        self.indent -= 1

    def visit_block(self, block):
        self.print_indent()
        print('Block')
        self.indent += 1
        for e in block.exprs:
            self.visit(e)
        self.indent -= 1

    def visit_literal(self, lit):
        self.print_indent()
        print(repr(lit.value))

    def visit_assignment(self, a):
        self.print_indent()
        print("Assignment")
        self.visit(a.name)
        self.indent += 1
        self.visit(a.expr)
        self.indent -= 1

    def visit_declaration(self, a):
        self.print_indent()
        print("Declaration")
        self.visit(a.name)
        self.indent += 1
        self.visit(a.expr)
        self.indent -= 1

    def visit_name(self, a):
        self.print_indent()
        print(f"<{a.name} {a.binding_depth}>")

    def visit_commented_expr(self, e):
        self.print_indent()
        print("Commented Expr")
        self.indent += 1
        self.visit(e.expr)
        self.visit(e.comment)
        self.indent -= 1

    def visit_while_expr(self, e):
        self.print_indent()
        print("While")
        self.indent += 1
        self.visit(e.cond)
        self.visit(e.body)
        self.indent -= 1

    def visit_bin_expr(self, e):
        self.print_indent()
        print("BinExpr", e.op)
        self.indent += 1
        self.visit(e.lhs)
        self.visit(e.rhs)
        self.indent -= 1
        
    def visit_unary_expr(self, e):
        self.print_indent()
        print("Unary", e.op)
        self.indent += 1
        self.visit(e.expr)
        self.indent -= 1

    def visit_if_expr(self, e):
        self.print_indent()
        print("If")
        self.indent += 1
        self.visit(e.cond)
        self.visit(e.then_body)
        if e.else_body is not None:
            self.visit(e.else_body)
        self.indent -= 1

    def visit_call(self, e):
        self.print_indent()
        print("Call")
        self.indent += 1
        self.visit(e.f)
        for a in e.args:
            self.visit(a)
        self.indent -= 1

    def visit_fn_expr(self, f):
        self.print_indent()
        print("Function", list(map(lambda n: n.name, f.params)))
        self.indent += 1
        self.visit(f.body)
        self.indent -= 1

    def visit_explain_expr(self, e):
        self.print_indent()
        print("Explain")
        self.indent += 1
        self.visit(e.expr)
        self.indent -= 1


class LiteralTransformer(visitors.Transformer):
    def __init__(self, filename, line_offset=0):
        super().__init__()
        self.filename = filename
        self.line_offset = line_offset

    def _source_loc(self, tree):
        return SourceLocation.from_tree(tree, self.filename, self.line_offset)

    def COMMENT(self, c):
        return Literal(self._source_loc(c), JlComment(c[2:-2]))

    def SIGNED_NUMBER(self, x):
        return Literal(self._source_loc(x), JlNumber(float(x)))

    def ESCAPED_STRING(self, s):
        return Literal(self._source_loc(s), JlString(s[1:-1]))

    def TRUE(self, t):
        return Literal(self._source_loc(t), JlBool(True))

    def FALSE(self, f):
        return Literal(self._source_loc(f), JlBool(False))

    def unit(self, u):
        return Literal(self._source_loc(u[0]), JlUnit())

    def CNAME(self, n):
        return str(n)


# would have been nice to use lark.ast_utils for this, but I couldn't figure out how to
# maintain line and column number when using it
class ToAst(visitors.Interpreter):
    def __init__(self, filename, line_offset=0):
        super().__init__()
        self.filename = filename
        self.line_offset = line_offset

    def _source_loc(self, tree):
        return SourceLocation.from_tree(tree, self.filename, self.line_offset)

    def __default__(self, tree):
        assert False
        
    def program(self, tree):
        return Program(self._source_loc(tree), self.visit_children(tree))

    def block(self, tree):
        return Block(self._source_loc(tree), self.visit_children(tree))

    def commented_expr(self, tree):
        return CommentedExpr(self._source_loc(tree), *self.visit_children(tree))

    def bin_expr(self, tree):
        return BinExpr(self._source_loc(tree),
                       *self.visit_children(tree))

    def unary_expr(self, tree):
        return UnaryExpr(self._source_loc(tree),
                         *self.visit_children(tree))

    def name(self, tree):
        return Name(self._source_loc(tree), *self.visit_children(tree))

    def assignment(self, tree):
        return Assignment(self._source_loc(tree), *self.visit_children(tree))

    def declaration(self, tree):
        return Declaration(self._source_loc(tree), *self.visit_children(tree))

    def if_expr(self, tree):
        return IfExpr(self._source_loc(tree), *self.visit_children(tree))

    def while_expr(self, tree):
        return WhileExpr(self._source_loc(tree), *self.visit_children(tree))

    def call_expr(self, tree):
        children = self.visit_children(tree)
        return CallExpr(self._source_loc(tree), children[0], children[1:])

    def fn_expr(self, tree):
        children = self.visit_children(tree)
        return FnExpr(self._source_loc(tree), children[:-1], children[-1])

    def explain_expr(self, tree):
        return ExplainExpr(self._source_loc(tree), *self.visit_children(tree))
