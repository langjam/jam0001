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
        return  # The math Node

    @_("expr MINUS expr")
    def expr(self, p):
        return  # The math Node

    @_("expr MULTIPLY expr")
    def expr(self, p):
        return  # The math Node

    @_("expr DIVIDE expr")
    def expr(self, p):
        return  # The math Node

    @_("expr MODULO expr")
    def expr(self, p):
        return  # The math Node

    @_("LPAREN expr RPAREN")
    def expr(self, p):
        return p.expr

    @_("expr EQ expr")
    def expr(self, p):
        return  # Equal Node

    @_("expr NOTEQ expr")
    def expr(self, p):
        return  # Not Equal Node

    @_("expr GT expr")
    def expr(self, p):
        return  # Greater Than Node

    @_("expr GEQT expr")
    def expr(self, p):
        return  # Greater or equals than Node

    @_("expr ST expr")
    def expr(self, p):
        return  # Smaller than Node

    @_("expr SEQT expr")
    def expr(self, p):
        return  # Smaller or equals than Node
