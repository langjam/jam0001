class ParserException(Exception):
  def __init__(self, token, msg):
    msg = token.filename + " Line " + str(token.line) + " Column " + str(token.col) + ": " +  msg
    super().__init__(msg)

class StackTrace:
  def __init__(self, token, error, nested = None):
    self.token = token
    self.error = error
    self.next = nested
