class ParserException(Exception):
  def __init__(self, token, msg):
    msg = token.filename + " Line " + str(token.line) + " Column " + str(token.col) + ": " +  msg
    super().__init__(msg)
