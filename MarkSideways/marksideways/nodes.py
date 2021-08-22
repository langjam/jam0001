from .exceptions import *
from .util import canonicalize_identifier
from .values import *
from .opmatrix import perform_op

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
  def __init__(self, first_token, name, class_def):
    super().__init__(first_token)
    self.name = name
    self.class_def= class_def
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
    
    if is_null(root_value): return to_null_error_value(root_value)

    if root_value.is_invocable:
      return run_function_value(self.open_paren, root_value, scope.globals, args)
    else:
      return new_error_value(self.expression.first_token, "This is not a function method or constructor and cannot be invoked like this.")

def run_function_value(throw_token, func_value, scope_globals, args):
  if func_value.type == 'FUNCTION':
    method_def = func_value.method_def
    return run_function(throw_token, method_def.code, scope_globals, None, method_def.arg_names, args)
  
  if func_value.type == 'METHOD':
    method_def = func_value.method_def
    class_def = method_def.class_def
    return run_function(throw_token, method_def.code, scope_globals, func_value.instance, method_def.arg_names, args)
  
  if func_value.type == 'CONSTRUCTOR':
    class_def = func_value.class_def
    instance = InstanceValue(func_value.class_def)
    value = run_function(throw_token, class_def.code, scope_globals, instance, class_def.arg_names, args)
    if value.is_error: return value
    return instance
  
  if func_value.type == 'BUILTIN_FUNCTION':
    output = func_value.handler(throw_token, args)
    if output == None: output = NULL_VALUE
    return output

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

class IfStatement(Executable):
  def __init__(self, if_token, condition, if_code, else_code):
    super().__init__(if_token)
    self.condition = condition
    self.if_code = if_code
    self.else_code = else_code
  def run(self, scope):
    condition = self.condition.run(scope)
    if condition.is_error: return error_status_from_value(condition)
    if is_null(condition): return to_null_error_status(self.condition.first_token)
    if condition.type != 'BOOL': new_error_status(self.condition.first_token, "If statement requires a boolean value for its condition.")
    if condition.value:
      run_this = self.if_code
    else:
      run_this = self.else_code
    return run_code_block(run_this, scope)

class ArrayDefinition(Expression):
  def __init__(self, first_token, expressions):
    super().__init__(first_token)
    self.expressions = expressions
  def run(self, scope):
    items = []
    for item in self.expressions:
      value = item.run(scope)
      if value.is_error: return value
      items.append(value)
    return ArrayValue(items)

class DictionaryDefinition(Expression):
  def __init__(self, first_token, keys, values):
    super().__init__(first_token)
    self.keys = keys
    self.values = values
  def run(self, scope):
    keys = []
    values = []
    d = DictionaryValue()
    for i in range(len(self.keys)):
      key_token = self.keys[i].first_token
      key = self.keys[i].run(scope)
      if key.is_error: return key
      value = self.values[i].run(scope)
      if value.is_error: return value
      is_collision = d.set_item(key_token, key, value)
      if is_collision:
        return new_error_value(key_token, "This dictionary definition contains a key collision.")
    return d

class DotField(Expression):
  def __init__(self, expression, dot, field_name):
    super().__init__(expression.first_token)
    self.root = expression
    self.dot = dot
    self.field_name = field_name
    self.canonical_field_name = canonicalize_identifier(field_name.value)

  def run(self, scope):
    value = self.root.run(scope)
    if value.is_error: return value
    if is_null(value): return to_null_error_value(self.dot)
    field_name = self.field_name.value
    if value.type == 'STRING':
      if field_name == 'length':
        return get_integer_value(len(value.value))
      else:
        return new_error_value(self.dot, "Strings do not have a field called '" + field_name + "'.")
    elif value.type == 'INSTANCE':
      output = value.fields.get(self.canonical_field_name)
      if output != None: return output
      output = value.class_def.methods.get(self.canonical_field_name)
      if output != None:
        return MethodValue(value, output)
      return new_error_value(self.dot, "This instance of " + value.class_def.name + " does not have a field called + '" + field_name + "'.")
    elif value.type == 'ARRAY':
      if field_name == 'length':
        return get_integer_value(len(value.value))
      else:
        field = value.get_field(field_name)
        if field != None: return field
        return new_error_value(self.dot, "Arrays do not have a field called '" + field_name + "'.")
    elif value.type == 'DICTIONARY':
      if field_name == 'length':
        return get_integer_value(len(value.keys))
      else:
        field = value.get_field(field_name)
        if field != None: return field
        return new_error_value(self.dot, "Dictionaries do not have a field called '" + field_name + "'.")
    elif value.type == 'CLASS':
      if field_name == 'init':
        return ConstructorValue(value.class_def)
      return new_error_value(self.dot, "The class " + value.class_def.name + " does not have a field called "+ field_name + ". Did you meant to call this on an instance?")
    else:
      return new_error_value(self.dot, "This value does not have a field called '" + field_name + "'.")

class BracketIndex(Expression):
  def __init__(self, root_expression, bracket_token, index_expression):
    super().__init__(root_expression.first_token)
    self.root = root_expression
    self.bracket_token = bracket_token
    self.index = index_expression
  def run(self, scope):
    root_value = self.root.run(scope)
    if root_value.is_error: return root_value
    if is_null(root_value): return to_null_error_value(self.bracket_token)
    index_value = self.index.run(scope)
    if index_value.is_error: return index_value
    if is_null(index_value): return to_null_error_value(self.index.first_token)
    if root_value.type == 'ARRAY':
      if index_value.type != 'INT': return new_error_value("Can only index into an array using an integer.")
      index = index_value.value
      if index < 0 or index >= len(root_value.value):
        return new_error_value(self.bracket_token, "Array index out of bounds! Index was " + str(index) + " but the length of the array is " + str(len(root_value.value)) + ".")
      return root_value.value[index]
    elif root_value.type == 'DICTIONARY':
      output = root_value.get_item(self.bracket_token, index_value)
      if output == None:
        return new_error_value("The index '" + index_value.to_string() + "' does not exist in this dictionary.")
      return output
    else:
      return new_error_value(self.bracket_token, "Using square brackets to index into a " + root_value.type + " type is not allowed.")
class InlineIncrement(Expression):
  def __init__(self, first_token, op_token, expression, is_prefix, is_increment):
    super().__init__(first_token)
    self.op_token = op_token
    self.root = expression
    self.is_prefix = is_prefix
    self.is_increment = is_increment
  def run(self, scope):
    root_type = None
    if isinstance(self.root, Variable):
      root_type = 'VAR'
      starting_value = self.root.run(scope)
    elif isinstance(self.root, BracketIndex):
      bracket_root = self.root.root.run(scope)
      if bracket_root.is_error: return bracket_root
      if is_null(bracket_root): return to_null_error_value(self.root.bracket_token)
      bracket_index = self.root.index.run(scope)
      if bracket_index.is_error: return bracket_index
      if is_null(bracket_index): return to_null_error_value(self.root.bracket_token)
      if bracket_root.type == 'ARRAY':
        root_type = 'ARRAY_INDEX'
        if bracket_index.type != 'INT': return new_error_value("Can only index into an array using an integer.")
        index = bracket_index.value
        if index < 0 or index >= len(bracket_root.value):
          return new_error_value(self.root.bracket_token, "Array index out of bounds! Index was " + str(index) + " but the length of the array is " + str(len(bracket_root.value)) + ".")
        starting_value = bracket_root.value[index]
      elif bracket_root.type == 'DICTIONARY':
        root_type = 'DICTIONAR_KEY'
        key = bracket_index.value
        starting_value = bracket_root.get_item(self.root.bracket_token, bracket_index)
      else:
        root_type = None
    elif isinstance(self.root, DotField):
      root_type = 'FIELD'
      obj_root = self.root.root.run(scope)
      if obj_root.type != 'INSTANCE':
        if obj_root.is_error: return obj_root
        if is_null(obj_root): return to_null_error_value(self.root.dot)
        root_type = None
      else:
        starting_value = obj_root.fields.get(self.root.canonical_field_name)

    if root_type == None:
      return new_error_value(self.op_token, "The " + self.op_token.value + " op is not support on this sort of expression.")

    if starting_value.is_error: return starting_value
    if is_null(starting_value): return to_null_error_value(starting_value)
    if starting_value.type != 'INT': return new_error_value(self.op_token, "Can only use " + self.op_token.value + " on integers. This is a " + starting_value.type + ".")

    n_before = starting_value.value
    n_after = n_before + (1 if self.is_increment else -1)
    value_after = get_integer_value(n_after)
    expr_value = value_after if self.is_prefix else starting_value

    if root_type == 'VAR':
      scope.locals[self.root.canonical_name] = value_after
    elif root_type == 'ARRAY_INDEX':
      bracket_root.value[index] = value_after
    elif root_type == 'DICTIONAR_KEY':
      bracket_root.set_item(self.root.bracket_token, bracket_index, value_after)
    elif root_type == 'FIELD':
      obj_root.fields[self.root.canonical_field_name] = value_after

    return expr_value


class AssignStatement(Executable):
  def __init__(self, target_expression, assign_op, value_expression):
    super().__init__(target_expression.first_token)
    self.target = target_expression
    self.assign_op = assign_op
    self.value = value_expression
    self.incremental_effective_op = assign_op.value[:-1] # pop off the = at the end so things like += become the string + for opmatrix lookups

  def run(self, scope):
    value = self.value.run(scope)
    is_direct_assign = self.assign_op.value == '='
    if value.is_error: return error_status_from_value(value)
    if isinstance(self.target, Variable):
      if is_direct_assign:
        scope.locals[self.target.canonical_name] = value
      else:
        original_value = self.target.run(scope)
        if original_value.is_error: return error_status_from_value(original_value)
        new_value = perform_op(self.assign_op, original_value, self.incremental_effective_op, value)
        if new_value.is_error: return error_status_from_value(new_value)
        scope.locals[self.target.canonical_name] = new_value
    elif isinstance(self.target, DotField):
      root = self.target.root.run(scope)
      field_name = self.target.canonical_field_name
      if root.is_error: return error_status_from_value(root)
      if is_null(root): return to_null_error_status(self.target.dot)
      if root.type == 'INSTANCE':
        if is_direct_assign:
          root.fields[field_name] = value
          return None
        else:
          original_value = root.fields.get(field_name)
          if original_value == None: return new_error_status(self.assign_op, "This instance has no pre-existing value for " + self.target.field_name.value)
          new_value = perform_op(self.assign_op, original_value, self.incremental_effective_op, value)
          if new_value.is_error: return error_status_from_value(new_value)
          root.fields[field_name] = new_value
      else:
        return new_error_status(self.assign_op, "Assigning a field to this type of value is not supported.")
    elif isinstance(self.target, BracketIndex):
      root = self.target.root.run(scope)
      if root.is_error: return error_status_from_value(root)
      if is_null(root): return to_null_error_status(self.target.bracket_token)

      index = self.target.index.run(scope)
      if index.is_error: return error_status_from_value(index)
      if is_null(index): return to_null_error_status(self.target.bracket_token)
      if root.type == 'ARRAY':
        if index.type != 'INT': return new_error_status(self.target.index.first_token, "Array index must be an integer but found a " + index.type + " instead.")
        arr = root.value
        i = index.value
        if i < 0 or i >= len(arr): return new_error_status(self.target.bracket_token, "Array index out of bounds. Index was " + str(i) + " but array length is " + str(len(arr)) + ".")
        if is_direct_assign:
          arr[i] = value
        else:
          original_value = root.value[i]
          new_value = perform_op(self.assign_op, original_value, self.incremental_effective_op, value)
          if new_value.is_error: return error_status_from_value(new_value)
          arr[i] = new_value
        return None
      elif root.type == 'DICTIONARY':
        if is_direct_assign:
          root.set_item(self.target.bracket_token, index, value)
        else:
          original_value = root.get_item(self.target.bracket_token, index)
          if original_value == None: return new_error_status(self.assign_op, "That key could not be found.")
          new_value = perform_op(self.assign_op, original_value, self.incremental_effective_op, value)
          if new_value.is_error: return error_status_from_value(new_value)
          root.set_item(self.target.bracket_token, index, new_value)
        return None
      else:
        return new_error_status(self.target.bracket_token, "Cannot assign an index on a " + root.type + " type.")
    else:
      return new_error_status(self.assign_op, "Cannot assign to this type of expression.")

_SHORT_CIRCUIT_OPS = {}
for t in ['&&', '||', '??']:
  _SHORT_CIRCUIT_OPS[t] = True

class OpChain(Expression):
  def __init__(self, expressions, ops):
    super().__init__(expressions[0].first_token)
    self.expressions = expressions
    self.ops = ops
  def run(self, scope):
    expression = self.expressions[0].run(scope)
    if expression.is_error: return expression
    left = expression
    for i in range(1, len(self.expressions)):
      op = self.ops[i - 1]
      op_value = op.value
      if _SHORT_CIRCUIT_OPS.get(op_value, False):
        if op_value == '&&' and left.type == 'BOOL' and not left.value:
          return FALSE_VALUE
        if op_value == '||' and left.type == 'BOOL' and left.value:
          return TRUE_VALUE
        if op_value == '??' and left.type != 'NULL':
          return left
      right = self.expressions[i].run(scope)
      if right.is_error: return right
      combined = perform_op(op, left, op_value, right)
      if combined.is_error: return combined
      left = combined
    return left
    

class ThisConstant(Expression):
  def __init__(self, first_token):
    super().__init__(first_token)
  def run(self, scope):
    if scope.this_context == None:
      return new_error_value(self.first_token, "The 'this' constant cannot be used in this context. It may only be used inside a class constructor or method.")
    return scope.this_context

class NullConstant(Expression):
  def __init__(self, first_token):
    super().__init__(first_token)
  def run(self, scope):
    return NULL_VALUE

class BooleanConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value
    self.cached_value = TRUE_VALUE if value else FALSE_VALUE
  def run(self, scope):
    return self.cached_value

class IntegerConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value
    self.cached_value = None
  def run(self, scope):
    if self.cached_value == None:
      self.cached_value = get_integer_value(self.value)
    return self.cached_value

class FloatConstant(Expression):
  def __init__(self, first_token, value):
    super().__init__(first_token)
    self.value = value
    self.cached_value = None
  def run(self, scope):
    if self.cached_value == None:
      self.cached_value = FloatValue(self.value)
    return self.cached_value

class UnaryPrefix(Expression):
  def __init__(self, op, root):
    super().__init__(op)
    self.root = root
  def run(self, scope):
    root_value = self.root.run(scope)
    if root_value.is_error: return root_value
    if is_null(root_value): return to_null_error_value(self.first_token)
    op = self.first_token.value
    if op == '!':
      if root_value.type != 'BOOL':
        return new_error_value(self.first_token, "Cannot apply a ! operator to a type of " + root_value.type + ".")
      return FALSE_VALUE if root_value.value else TRUE_VALUE
    
    if root_value.type == 'FLOAT' and op == '-':
      return FloatValue(-root_value.value)
    if root_value.type != 'INT':
      return new_error_value(self.first_token, "Cannot apply the " + op + " operator to a type of " + root_value.type + ".")
    if op == '-':
      return get_integer_value(-root_value.value)
      
    return get_integer_value(~root_value.value)


class WhileLoop(Executable):
  def __init__(self, while_token, condition, code):
    super().__init__(while_token)
    self.condition = condition
    self.code = code
  def run(self, scope):
    while True:
      condition = self.condition.run(scope)
      if condition.is_error: return error_status_from_value(condition)
      if condition.type != 'BOOL': return new_error_status(self.condition.first_token, "while loops must have a boolean as their condition.")
      if not condition.value: return None

      result = run_code_block(self.code, scope)
      if result != None:
        if result.type == 'EXCEPTION': return result
        if result.type == 'BREAK': return None
        if result.type == 'RETURN': return result

class DoWhileLoop(Executable):
  def __init__(self, do_token, code, condition):
    super().__init__(do_token)
    self.code = code
    self.condition = condition
  def run(self, scope):
    while True:
      result = run_code_block(self.code, scope)
      if result != None:
        if result.type == 'EXCEPTION': return result
        if result.type == 'BREAK': return None
        if result.type == 'RETURN': return result
      
      condition = self.condition.run(scope)
      if condition.is_error: return error_status_from_value(condition)
      if condition.type != 'BOOL': return new_error_status(self.condition.first_token, "do-while loops must have a boolean as their condition.")
      if not condition.value: return None


class ForLoop(Executable):
  def __init__(self, for_token, variable_token, loop_op_token, start_expr, end_expr, code):
    super().__init__(for_token)
    self.variable = variable_token
    self.variable_id = canonicalize_identifier(variable_token.value)
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
    end_trigger = end
    if self.is_inclusive: end_trigger += step
    i = start
    while i != end_trigger:
      scope.locals[self.variable_id] = get_integer_value(i)
      status = run_code_block(self.code, scope)
      if status != None:
        if status.type == 'BREAK':
          return None
        elif status.type == 'CONTINUE':
          pass # just let it continue on its own
        else: # RETURN or EXCEPTION
          return status

      i += step
class TernaryExpression(Expression):
  def __init__(self, condition, question_mark, true_value, false_value):
    super().__init__(condition.first_token)
    self.condition = condition
    self.question_mark = question_mark
    self.true_value = true_value
    self.false_value = false_value
  def run(self, scope):
    value = self.condition.run(scope)
    if value.is_error: return value
    if is_null(value): return to_null_error_value(self.condition.first_token)
    if value.type != 'BOOL': return new_error_value(self.question_mark, "Ternary conditions can only use boolean values as their condition.")
    output = self.true_value if value.value else self.false_value
    return output.run(scope)
    
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

def to_null_error_value(token):
  return new_error_value(token, "Null reference error!")

def to_null_error_status(token):
  return error_status_from_value(to_null_error_value(token))

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
