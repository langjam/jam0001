from .exceptions import *
from .util import canonicalize_identifier
from .values import *

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
  
  def run(self, scope):
    raise ParserException(self.first_token, ".run() not implemented for " + str(self))

class Executable(Node):
  def __init__(self, first_token):
    super().__init__(first_token)
  
  def run(self, scope):
    raise ParserException(self.first_token, ".run() not implemented for " + str(self))

class Variable(Expression):
  def __init__(self, first_token, name):
    super().__init__(first_token)
    self.name = name
    self.canonical_name = canonicalize_identifier(name)
  
  def run(self, scope):
    print(scope)
    if scope.locals.get(self.canonical_name) != None:
      value = scope.locals[self.canonical_name]
    elif scope.globals.get(self.canonical_name) != None:
      value = scope.globals[self.canonical_name]
    else:
      return new_error_value(self.first_token, "The variable '" + self.name + "' is used but has not been defined yet.")
    
    return value

class StringConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value
    self.cached_value = None
  def run(self, scope):
    if self.cached_value == None:
      self.cached_value = StringValue(self.value)
    return self.cached_value

class FunctionInvocation(Expression):
  def __init__(self, root_expression, open_paren_token, args):
    super().__init__(root_expression.first_token)
    self.expression = root_expression
    self.open_paren = open_paren_token
    self.args = args
  
  def run(self, scope):
    root_value = self.expression.run(scope)
    if root_value.is_error: return root_value
    args = []
    for arg in self.args:
      arg_value = arg.run(scope)
      if arg_value.is_error: return arg_value
      args.append(arg_value)
    
    if is_null(root_value): return to_null_error(root_value)

    if root_value.type == 'FUNCTION':
      raise Exception("TODO: Invoking a function definition")
    elif root_value.type == 'METHOD':
      raise Exception("TODO: Invoking an instance method")
    elif root_value.type == 'CONSTRUCTOR':
      class_def = root_value.class_def
      instance = InstanceValue(root_value.class_def)
      value = run_function(self.open_paren, class_def.code, scope.globals, instance, class_def.arg_names, args)
      if value.is_error: return value
      return instance
    elif root_value.type == 'BUILTIN_FUNCTION':
      return root_value.handler(args)
    else:
      return new_error_value(self.expression.first_token, "This is not a function method or constructor and cannot be invoked like this.")

class ExpressionAsExecutable(Executable):
  def __init__(self, expression):
    super().__init__(expression.first_token)
    self.expression = expression
  def run(self, scope):
    value = self.expression.run(scope)
    if value.is_error:
      return error_status_from_value(value)
    return None

class BreakStatement(Executable):
  def __init__(self, token):
    super().__init__(token)
  def run(self, scope):
    return StatusWrapper('BREAK', None)

class ContinueStatement(Executable):
  def __init__(self, token):
    super().__init__(token)
  def run(self, scope):
    return StatusWrapper('CONTINUE', None)

class ReturnStatement(Executable):
  def __init__(self, return_token, expression):
    super().__init__(return_token)
    self.expression = expression
  def run(self, scope):
    if self.expression == None:
      value = NULL_VALUE
    else:
      value = self.expression.run(scope)
    if value.is_error: return error_status_from_value(value)
    return StatusWrapper('RETURN', value)

class DotField(Expression):
  def __init__(self, expression, dot, field_name):
    super().__init__(expression.first_token)
    self.root = expression
    self.dot = dot
    self.field_name = field_name

  def run(self, scope):
    value = self.root.run(scope)
    if value.is_error: return value
    if is_null(value): return to_null_error(value)
    field_name = self.field_name.value
    if value.type == 'STRING':
      if field_name == 'length':
        return get_integer_value(len(value.value))
      else:
        return new_error_value(self.dot, "Strings dot not have a field called '" + field_name + "'.")
    elif value.type == 'CLASS':
      if field_name == 'init':
        return ConstructorValue(value.class_def)
      return new_error_value(self.dot, "The class " + value.class_def.name + " does not have a field called "+ field_name + ". Did you meant to call this on an instance?")
    else:
      return new_error_value(self.dot, "This value does not have a field called '" + field_name + "'.")

class BracketIndex(Expression):
  def __init__(self, root_expression, bracket_token, index_expression):
    super().__init__(self, root_expression.first_token)
    self.root = root_expression
    self.bracket_token = bracket_token
    self.index = index_expression
  
class AssignStatement(Executable):
  def __init__(self, target_expression, assign_op, value_expression):
    super().__init__(target_expression.first_token)
    self.target = target_expression
    self.assign_op = assign_op
    self.value = value_expression

  def run(self, scope):
    value = self.value.run(scope)
    if value.is_error: return error_status_from_value(value)
    if isinstance(self.target, Variable):
      if self.assign_op.value == '=':
        scope.locals[canonicalize_identifier(self.target.name.value)] = value
      else:
        raise Exception("TODO: incremental assignment")
    elif isinstance(self.target, DotField):
      root = self.target.root.run(scope)
      if root.is_error: return error_status_from_value(root)
      if is_null(root): return to_null_error(self.target.bracket_token)
      raise Exception("TODO: assign to a dot field")
    elif isinstance(self.target, BracketIndex):
      root = self.target.root.run(scope)
      if root.is_error: return error_status_from_value(root)
      if is_null(root): return to_null_error(self.target.dot)
      raise Exception("TODO: assign to a bracket index")
    else:
      return new_error_status(self.assign_op, "Cannot assign to this type of expression.")

class OpChain(Expression):
  def __init__(self, expressions, ops):
    super().__init__(expressions[0].first_token)
    self.expressions = expressions
    self.ops = ops

class ThisConstant(Expression):
  def __init__(self, first_token):
    super().__init__(first_token)
  def run(self, scope):
    if scope.this_context == None:
      return new_error_value(self.first_token, "The 'this' constant cannot be used in this context. It may only be used inside a class constructor or method.")
    return scope.this_context

class IntegerConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value
    self.cached_value = None
  def run(self, scope):
    if self.cached_value == None:
      self.cached_value = get_integer_value(self.value)
    return self.cached_value

class ForLoop(Executable):
  def __init__(self, for_token, variable_token, loop_op_token, start_expr, end_expr, code):
    super().__init__(for_token)
    self.variable = variable_token
    self.loop_op = loop_op_token
    self.is_inclusive = self.loop_op.value == 'thru'
    self.start_expr = start_expr
    self.end_expr = end_expr
    self.code = code

  def run(self, scope):
    start_num = self.start_expr.run(scope)
    if start_num.is_error: return error_status_from_value(start_num)
    if start_num.type != 'INT': return new_error_status(self.start_expr.first_token, "for loop starting expression must be an integer.")
    end_num = self.end_expr.run(scope)
    if end_num.is_error: return error_status_from_value(end_num)
    if end_num.type != 'INT': return new_error_status(self.end_expr.first_token, "for loop ending expression must be an integer.")
    start = start_num.value
    end = end_num.value
    if start < end:
      step = 1
    else:
      step = -1
    end_trigger = end_num
    if self.is_inclusive: end_trigger += step
    i = start
    var_name = canonicalize_identifier(self.variable.name.value)
    while i != end_trigger:
      scope.locals[var_name] = get_integer_value(i)
      status = run_code_block(self.code, scope)
      if status != None:
        if status.type == 'BREAK':
          return None
        elif status.type == 'CONTINUE':
          pass # just let it continue on its own
        else: # RETURN or EXCEPTION
          return status

      i += step
    

def _add_code_impl(code_lines, text, line_offset):
    # This is a little hacky, but basically, empty lines are added to code_lines
    # until it corresponds to the real document. Makes the tokenizer simpler.
    # This is a hackathon, after all.
    while len(code_lines) < line_offset:
      code_lines.append('')
    for line in text.rstrip().split('\n'):
      code_lines.append(line)

def is_null(value):
  return value.type == 'NULL'

def to_null_error(token):
  return new_error_value(token, "Null reference error!")

def run_function(throw_token, code_lines, global_scope, this_context, arg_names, arg_values):
  if len(arg_names) != len(arg_values):
    return new_error_value(throw_token, "Incorrect number of arguments supplied. Expected " + str(arg_names) + " but found " + str(arg_values) + ".")
  new_locals = {}
  for i in range(len(arg_names)):
    new_locals[canonicalize_identifier(arg_names[i])] = arg_values[i]
  result = run_code_block(code_lines, Scope(global_scope, new_locals, this_context))
  if result != None:
    if result.type == 'EXCEPTION':
      stack_trace = result.arg
      return ErrorValue(StackTrace(throw_token, None, stack_trace))
    if result.type == 'RETURN':
      return result.arg
  return NULL_VALUE
   
def run_code_block(lines, scope):
  i = 0
  while i < len(lines):
    line = lines[i]
    status = line.run(scope)
    if status != None:
      return status
    i += 1
  return None

class Scope:
  def __init__(self, globals, locals, this_context = None):
    self.globals = globals
    self.locals = locals
    self.this_context = this_context
