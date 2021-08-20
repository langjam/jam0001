from sly import Parser

from lexer import ComstructLexer
from application_stack_utils import StatementNode


class ComstructParser(Parser):
    tokens = ComstructLexer.tokens

    @_("expr NEWSTMT")
    def exprs(self, p):
        return [p.expr, ]

    @_("exprs exprs")
    def exprs(self, p):
        return p.exprs0 + p.exprs1

    @_("NUMBER")
    def expr(self, p):
        return StatementNode.LiterallyNode(p.NUMBER)

    @_("NAME")
    def expr(self, p):
        return StatementNode.VarNode(p.NAME)

    @_("expr PLUS expr")
    def expr(self, p):
        return StatementNode.MathNode("+", p.expr0, p.expr1)

    @_("expr MINUS expr")
    def expr(self, p):
        return StatementNode.MathNode("-", p.expr0, p.exp1)

    @_("expr MULTIPLY expr")
    def expr(self, p):
        return StatementNode.MathNode("*", p.expr0, p.expr1)

    @_("expr DIVIDE expr")
    def expr(self, p):
        return StatementNode.MathNode("/", p.expr0, p.expr1)

    @_("expr MODULO expr")
    def expr(self, p):
        return StatementNode.MathNode("%", p.expr0, p.expr1)

    @_("LPAREN expr RPAREN")
    def expr(self, p):
        return p.expr

    @_("expr EQ expr")
    def expr(self, p):
        return StatementNode.EqualNode(p.expr0, p.expr1)

    @_("expr NOTEQ expr")
    def expr(self, p):
        return StatementNode.NotEqualNode(p.expr0, p.expr1)

    @_("expr GT expr")
    def expr(self, p):
        return StatementNode.GreaterThanNode(p.expr0, p.expr1)

    @_("expr GEQT expr")
    def expr(self, p):
        return StatementNode.GreaterOrEqualsThanNode(p.expr0, p.expr1)

    @_("expr ST expr")
    def expr(self, p):
        return StatementNode.SmallerThanNode(p.expr0, p.expr1)

    @_("expr SEQT expr")
    def expr(self, p):
        return StatementNode.SmallerOrEqualsThanNode(p.expr0, p.expr1)
