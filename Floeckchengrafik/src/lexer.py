from sly import Lexer


# noinspection PyRedeclaration,PyMethodMayBeStatic
class ComstructLexer(Lexer):
    tokens = {
        "NAME",  # Any name like a var
        "NUMBER",  # A number like an int or a float
        "STRING",  # One or multible Lines of words
        "PLUS",  # +
        "MINUS",  # -
        "MULTIPLY",  # *
        "DIVIDE",  # /
        "MODULO",  # %
        "LPAREN",  # (
        "RPAREN",  # )
        "LBRACE",  # {
        "RBRACE",  # }
        "LBRACK",  # [
        "RBRACK",  # ]
        "EQ",  # ==
        "NOTEQ",  # !=
        "GT",  # Greater than >
        "GEQT",  # Greater or equals than >=
        "ST",  # Smaller than <
        "SEQT",  # Smaller or equals than <=
        "ASSIGN",  # Assign a vaiable (=)
        "NEWSTMT",  # ?
        "SEP",  # :
        "FUNCDESC",  # Define the args of a class / method /* @param */
        "FUNCSEP",  # .
    }

    ignore = " \t\n"
    ignore_comment = r"\\\\.*"

    FUNCDESC = r'/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/'
    FUNCSEP = r'\.'
    NAME = r'[a-zA-Z_][a-zA-Z_0-9]*'
    PLUS = r'\+'
    STRING = r'"[^\"^\n]+"|""'
    MINUS = r'-'
    MULTIPLY = r'\*'
    DIVIDE = r'/'
    MODULO = r'%'
    LPAREN = r'\('
    RPAREN = r'\)'
    LBRACE = r'\{'
    RBRACE = r'\}'
    LBRACK = r'\['
    RBRACK = r'\]'
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

    def STRING(self, t):
        t.value = t.value[1:-1]
        return t

    # noinspection PyUnresolvedReferences
    def FUNCDESC(self, t):
        # t.value = t.value.replace("*", "")
        token = t.value.removeprefix("/*").removesuffix("*/").replace("*", "").split("\n")
        t.value = FuncDescProcessor().tokenize(token)
        return t

    def error(self, t):
        print('[LexerError] Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1


class FuncDescProcessor:
    valid_tokens = {
        "param",
        "returns"
    }

    def tokenize(self, tokens):
        ret = []

        for token in tokens:
            token = token.replace(r"*", "").strip()
            if token.startswith("- "):
                token = token[2:]
                if token.split(" ")[0] in self.valid_tokens:
                    if token.startswith("param"):
                        token = ("param", token.removeprefix("param ").strip().split(" ")[0])
                    elif token.startswith("returns"):
                        token = ("returns", token.removeprefix("returns ").strip().split(" ")[0])
                    ret.append(token)

        return ret
