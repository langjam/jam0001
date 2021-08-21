from sly import Lexer


# noinspection PyRedeclaration,PyMethodMayBeStatic
class ComstructLexer(Lexer):
    tokens = {
        "NAME",  # Any name like a var
        "NUMBER",  # A number like an int or a float
        "PLUS",  # +
        "MINUS",  # -
        "MULTIPLY",  # *
        "DIVIDE",  # /
        "MODULO",  # %
        "LPAREN",  # (
        "RPAREN",  # )
        "LBRACE",  # {
        "RBRACE",  # }
        "EQ",  # ==
        "NOTEQ",  # !=
        "GT",  # Greater than >
        "GEQT",  # Greater or equals than >=
        "ST",  # Smaller than <
        "SEQT",  # Smaller or equals than <=
        "ASSIGN",  # Assign a vaiable (=)
        "NEWSTMT",  # ?
        "SEP",  # :
        "MLCOMMENT"  # \\ Text \\
    }

    ignore = " \t\n"
    ignore_comment = "comment:"

    MLCOMMENT = r"\\\\[\s\S]*\\\\"
    NAME = r'[a-zA-Z_][a-zA-Z_0-9]*'
    PLUS = r'\+'
    MINUS = r'-'
    MULTIPLY = r'\*'
    DIVIDE = r'/'
    MODULO = r'%'
    LPAREN = r'\('
    RPAREN = r'\)'
    LBRACE = r'\{'
    RBRACE = r'\}'
    EQ = r'=='
    NOTEQ = r'!='
    GT = r'>'
    GEQT = r'>='
    ST = r'<'
    SEQT = r'<='
    ASSIGN = r'='
    NEWSTMT = r'\?'
    SEP = r':'

    @_(r"\d+")
    def NUMBER(self, t):
        t.value = int(t.value)
        return t

    def NEWSTMT(self, t):
        self.lineno += len(t.value)
        return t

    def MLCOMMENT(self, t):
        t.value = t.value.replace("\\", "")
        return t

    def error(self, t):
        print('[LexerError] Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1

# 2? comment: just is an expression
# a = 3?
#
# comment: \\ This is a comment and not ඞ || AMOGUS || \\
#
# b = 3 + a? comment: \\ Should generate a tree like that: (VarAssignNode, b, (MathNode, +, 3, (VarNode, a))) \\
#
# c = 2?
#
# d = 10?
#
# the_answer_to_everything = d+d+d+d+c? comment: \\ 42 \\
#
# comment: \\ In the State we are currently working, you can only view everything via debugger.
# comment:    There might be an Update for this Test to print the final stuff to the terminal. \\
#
# out(the_answer_to_everything)?
