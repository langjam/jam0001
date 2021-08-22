def get_context(location, text):
    lines = text.split('\n')
    if location.line == location.end_line:
        line = lines[location.line - 1]
        marker = ' ' * (location.column - 1) + '^' + \
            '~' * (location.end_column - location.column - 1)
        return line + '\n' + marker
    else:
        first_line = lines[location.line - 1]
        last_line = lines[location.end_line - 1]
        marker = ' ' * (location.column - 1) + '^' + \
            '~' * (len(first_line) - location.column)
        end_marker = '~' * (location.end_column - 1)
        msg = first_line + '\n' + marker + '\n'
        if location.end_line != location.line + 1:
            msg += '...\n'
        msg += last_line + '\n' + end_marker
        return msg


def format_backtrace(backtrace, text):
    bt = "Backtrace (most recent call last):\n\n"
    for loc in backtrace:
        bt += f"{loc.filename}:{loc.line}:{loc.column}\n"
        bt += get_context(loc, text) + '\n'
    return bt


class JlException(Exception):
    def __init__(self, backtrace=None, location=None):
        self.backtrace = backtrace or []
        if location is not None:
            self.backtrace = self.backtrace + [location]

    def get_backtrace(self, text):
        return format_backtrace(self.backtrace, text) + '\n' + str(self)


class UnboundVariable(JlException):
    def __init__(self, name):
        super().__init__([], name.location)
        self.name = name

    def __str__(self):
        return f"unbound variable {self.name.name}"


class UninizializedVariable(JlException):
    def __init__(self, bt, name):
        super().__init__(bt, name.location)
        self.name = name

    def __str__(self):
        return f"error: variable {self.name.name} was accessed before it was fully initialized"


class JlTypeError(JlException):
    def __init__(self, msg, bt=None, expr=None):
        super().__init__(bt, expr)
        self.msg = msg

    def __str__(self):
        return f"type error: {self.msg}"
