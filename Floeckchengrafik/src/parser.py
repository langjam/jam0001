from sly import Parser

from lexer import ComstructLexer
from application_stack_utils import StatementNode


class CustomLog:
    def warning(self, *args):
        pass


# noinspection PyUnusedLocal
class ComstructParser(Parser):
    tokens = ComstructLexer.tokens

    # debugfile = "parser.log"

    log = CustomLog()

    precedence = (
        ("left", "NAME", "NUMBER", "STRING"),
        ("left", "PLUS", "MINUS"),
        ("left", "MULTIPLY", "DIVIDE"),
        ("left", "FUNCDESC"),
        ("left", "OR", "AND", "NOT"),
        ("left", "EQ", "NOTEQ", "GT", "GEQT", "ST", "SEQT")
    )

    @_("expr NEWSTMT")
    def exprs(self, p):
        return [p.expr, ]

    @_("exprs exprs")
    def exprs(self, p):
        return p.exprs0 + p.exprs1

    @_("LBRACE exprs RBRACE")
    def expr(self, p):
        return StatementNode.StoredProcedureNode(p.exprs)

    @_("LBRACE RBRACE")
    def expr(self, p):
        return StatementNode.StoredProcedureNode([])

    @_("NUMBER")
    def expr(self, p):
        return StatementNode.LiterallyNode(p.NUMBER)

    @_("STRING")
    def expr(self, p):
        return StatementNode.LiterallyNode(p.STRING)

    @_("NAME")
    def expr(self, p):
        return StatementNode.VarNode(p.NAME)

    @_("expr OR expr")
    def expr(self, p):
        return StatementNode.OperationNode("||", p.expr0, p.expr1)

    @_("expr AND expr")
    def expr(self, p):
        return StatementNode.OperationNode("&&", p.expr0, p.expr1)

    @_("NOT expr")
    def expr(self, p):
        return StatementNode.OperationNode("!!", p.expr, None)

    @_("expr PLUS expr")
    def expr(self, p):
        return StatementNode.OperationNode("+", p.expr0, p.expr1)

    @_("expr MINUS expr")
    def expr(self, p):
        return StatementNode.OperationNode("-", p.expr0, p.expr1)

    @_("expr MULTIPLY expr")
    def expr(self, p):
        return StatementNode.OperationNode("*", p.expr0, p.expr1)

    @_("expr DIVIDE expr")
    def expr(self, p):
        return StatementNode.OperationNode("/", p.expr0, p.expr1)

    @_("expr MODULO expr")
    def expr(self, p):
        return StatementNode.OperationNode("%", p.expr0, p.expr1)

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

    @_("NAME ASSIGN expr")
    def expr(self, p):
        return StatementNode.VarAssignNode(p.NAME, p.expr)

    @_("NAME LPAREN RPAREN")
    def expr(self, p):
        return StatementNode.FunctionCallNode(p.NAME, [])

    @_("LBRACK RBRACK")
    def expr(self, p):
        return StatementNode.LiterallyNode([])

    @_("LBRACK arglist RBRACK")
    def expr(self, p):
        def walk(walk_tree_func, node):
            newargs = []
            for elem in node.var:
                newargs.append(walk_tree_func(elem))

            node.var = newargs

        return StatementNode.LiterallyNode(p.arglist, walk_function=walk)

    @_('expr')
    def elem(self, p):
        return p.expr

    @_('NONE')
    def expr(self, p):
        return None

    @_('TRUE')
    def expr(self, p):
        return True

    @_('FALSE')
    def expr(self, p):
        return False

    @_('elem')
    def arglist(self, p):
        return [p.elem, ]

    @_('arglist SEP elem')
    def arglist(self, p):
        return p.arglist + [p.elem, ]

    @_("NAME FUNCSEP NAME LPAREN arglist RPAREN")
    def expr(self, p):
        return StatementNode.ClassMethodExecuteNode(p.NAME0, p.NAME1, p.arglist)

    @_("NAME FUNCSEP NAME LPAREN RPAREN")
    def expr(self, p):
        return StatementNode.ClassMethodExecuteNode(p.NAME0, p.NAME1, [])

    @_("NAME LPAREN arglist RPAREN")
    def expr(self, p):
        return StatementNode.FunctionCallNode(p.NAME, p.arglist)

    @_("FUNCDESC")
    def exprs(self, t):
        return [StatementNode.FunctionDescriptionNode(t.FUNCDESC), ]
