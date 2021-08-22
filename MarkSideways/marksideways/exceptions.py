class ParserException(Exception):
  def __init__(self, token, msg):
    msg = token.filename + " Line " + str(token.line) + " Column " + str(token.col) + ": " +  msg
    super().__init__(msg)

class StackTrace:
  def __init__(self, token, error, nested = None):
    if '.NullValue' in str(token): 
      raise Exception("WAAAAAA")
    self.token = token
    self.error = error
    self.next = nested

def print_stack_trace(stack_trace):
  stack_trace_walker = stack_trace
  while stack_trace_walker != None:
    token = stack_trace_walker.token
    error = stack_trace_walker.error
    prefix = token.filename + " Line " + str(token.line) + " Column " + str(token.col) + ":"
    print(prefix)
    if error != None:
      print("  > " + error)
    stack_trace_walker = stack_trace_walker.next
