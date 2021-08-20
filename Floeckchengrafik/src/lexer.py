from sly import Lexer


# noinspection PyRedeclaration
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
        "NEWSTMT"  # ?
    }

    ignore = " \t\n"
    ignore_comment = "comment: .*"

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

    def NEWSTMT(self, t):
        self.lineno += len(t.value)
        return t

    @_(
        r"\d+",  # Regular Int notation
        r"0x\d+",  # Hex int notation
        r"1x\d+",  # Binary int notation
    )
    def NUMBER(self, t):
        t.value = t.value.lower()
        if t.value.startswith("0x"):
            t.value = int(t.value.removeprefix("0x"), 16)
        elif t.value.startswith("1x"):
            t.value = int(t.value.removeprefix("1x"), 2)
        else:
            t.value = int(t.value)
        return t

    def error(self, t):
        print('[LexerError] Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1
