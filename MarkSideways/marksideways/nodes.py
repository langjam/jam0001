from .exceptions import ParserException
from .util import canonicalize_identifier

class Node:
  def __init__(self, first_token):
    self.first_token = first_token

class ProgramRoot(Node):
  def __init__(self, first_token, name):
    super().__init__(first_token)
    self.name = name
    self.methods = {}
    self.method_order = []
    self.classes = {}
    self.class_order = []
    self.code_lines = []
    self.code = []
  
  def add_class(self, class_def):
    name = canonicalize_identifier(class_def.name)
    if self.classes.get(name) != None:
      raise ParserException(class_def.first_token, "There are multiple classes with the name '" + name + "'.")
    self.classes[name] = class_def
    self.class_order.append(class_def)
  
  def add_method(self, method_def):
    name = canonicalize_identifier(method_def.name)
    if self.methods.get(name) != None:
      raise ParserException(method_def.first_token, "There are multiple methods in the global scope with the name '" + name + "'.")
    self.methods[name] = method_def
    self.method_order.append(method_def)

  def add_code(self, text, line_offset):
    _add_code_impl(self.code_lines, text, line_offset)

class ClassDefinition(Node):
  def __init__(self, first_token, name):
    super().__init__(first_token)
    self.name = name
    self.methods = {}
    self.method_order = []
    
    # These are for the constructor
    self.code_lines = []
    self.code = []
    self.arg_tokens = []
    self.arg_names = []
  
  def add_method(self, method_def):
    name = canonicalize_identifier(method_def.name)
    if self.methods.get(name) != None:
      raise ParserException(method_def.first_token, "There are multiple methods in the class " + self.name + " with the name '" + name + "'.")
    self.methods[name] = method_def
    self.method_order.append(method_def)
  
  def add_argument(self, token, name):
    self.arg_tokens.append(token)
    self.arg_names.append(name)

  def add_code(self, text, line_offset):
    _add_code_impl(self.code_lines, text, line_offset)

class MethodDefinition(Node):
  def __init__(self, first_token, name):
    super().__init__(first_token)
    self.name = name
    self.code_lines = []
    self.code = []
    self.arg_tokens = []
    self.arg_names = []

  def add_argument(self, token, name):
    self.arg_tokens.append(token)
    self.arg_names.append(name)

  def add_code(self, text, line_offset):
    _add_code_impl(self.code_lines, text, line_offset)

class Expression(Node):
  def __init__(self, first_token):
    super().__init__(first_token)

class Executable(Node):
  def __init__(self, first_token):
    super().__init__(first_token)

class Variable(Expression):
  def __init__(self, first_token, name):
    super().__init__(first_token)
    self.name = name

class StringConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value

class FunctionInvocation(Expression):
  def __init__(self, root_expression, open_paren_token, args):
    super().__init__(root_expression.first_token)
    self.expression = root_expression
    self.open_paren = open_paren_token
    self.args = args

class ExpressionAsExecutable(Executable):
  def __init__(self, expression):
    super().__init__(expression.first_token)
    self.expression = expression

class DotField(Expression):
  def __init__(self, expression, dot, field_name):
    super().__init__(expression.first_token)
    self.root = expression
    self.dot = dot
    self.field_name = field_name

class AssignStatement(Executable):
  def __init__(self, target_expression, assign_op, value_expression):
    super().__init__(target_expression.first_token)
    self.target = target_expression
    self.assign_op = assign_op
    self.value = value_expression

class OpChain(Expression):
  def __init__(self, expressions, ops):
    super().__init__(expressions[0].first_token)
    self.expressions = expressions
    self.ops = ops

class IntegerConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value

def _add_code_impl(code_lines, text, line_offset):
    # This is a little hacky, but basically, empty lines are added to code_lines
    # until it corresponds to the real document. Makes the tokenizer simpler.
    # This is a hackathon, after all.
    while len(code_lines) < line_offset:
      code_lines.append('')
    for line in text.rstrip().split('\n'):
      code_lines.append(line)

