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
