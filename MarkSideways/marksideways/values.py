# TODO: all the to_string methods for complex types need to have a cycle check

from .exceptions import *

class Value:
  def __init__(self, _type):
    self.type = _type
    self.is_error = False

class ErrorValue(Value):
  def __init__(self, stackTrace):
    super().__init__('ERROR')
    self.stackTrace = stackTrace
    self.is_error = True
  def to_string(self):
    raise Exception(".to_string() was called on ErrorValue.")

def new_error_value(token, msg):
  return ErrorValue(StackTrace(token, msg))

class NullValue(Value):
  def __init__(self):
    super().__init__('NULL')
  def to_string(self):
    return 'null'
NULL_VALUE = NullValue()

class BoolValue(Value):
  def __init__(self, value):
    super().__init__('BOOL')
    self.value = value
  def to_string(self):
    return 'true' if self.value else 'false'
TRUE_VALUE = BoolValue(True)
FALSE_VALUE = BoolValue(False)
def get_bool_value(value):
  if value: return TRUE_VALUE
  return FALSE_VALUE

class IntegerValue(Value):
  def __init__(self, value):
    super().__init__('INT')
    self.value = value
  def to_string(self):
    return str(self.value)
INTEGER_VALUES = {}
for i in (-1, 0, 1):
  INTEGER_VALUES[i] = IntegerValue(i)

def get_integer_value(n):
  if n < 2000 and n > -2000:
    value = INTEGER_VALUES.get(n)
    if value == None:
      value = IntegerValue(n)
      INTEGER_VALUES[n] = value
  else:
    value = IntegerValue(n)
  return value

class FloatValue(Value):
  def __init__(self, value):
    super().__init__('FLOAT')
    self.value = value
  def to_string(self):
    return str(self.value)

class StringValue(Value):
  def __init__(self, value):
    super().__init__('STRING')
    self.value = value
  def to_string(self):
    return self.value

class ArrayValue(Value):
  def __init__(self, value):
    super().__init__('ARRAY')
    self.value = value
    self.fields = {}
  def _builtin_add(self, throw_token, args):
    for arg in args:
      self.value.append(arg)
    return NULL_VALUE
  def _builtin_pop(self, throw_token, args):
    if len(args) != 0:
      return new_error_value(throw_token, "Array's .pop() method takes in no arguments.")
    if len(self.value) == 0:
      return new_error_value(throw_token, "Cannot call .pop() on an empty array.")
    value = self.value.pop()
    return value
  def get_field(self, name):
    output = self.fields.get(name)
    if output == None:
      if name == 'add':
        output = BuiltInFunction('array.add', self._builtin_add)
      if name == 'pop':
        output = BuiltInFunction('array.pop', self._builtin_pop)
      self.fields[name] = output
    return output
  def to_string(self):
    sb = ['[']
    for i in range(len(self.value)):
      if i > 0: sb.append(', ')
      sb.append(self.value[i].to_string())
    sb.append(']')
    return ''.join(sb)

class BuiltInFunction(Value):
  def __init__(self, id, handler):
    super().__init__('BUILTIN_FUNCTION')
    self.handler = handler
  def to_string(self):
    return "<Built-in function: " + self.id + ">"

class FunctionValue(Value):
  def __init__(self, method_def):
    super().__init__('FUNCTION')
    self.method_def = method_def
  def to_string(self):
    return "<function: " + self.method_def.name.value + ">"

class ClassValue(Value):
  def __init__(self, class_def):
    super().__init__('CLASS')
    self.class_def = class_def
  def to_string(self):
    return "<class: " + self.class_def.name.value + ">"

instance_counter = [1]
class InstanceValue(Value):
  def __init__(self, class_def):
    super().__init__('INSTANCE')
    self.class_def = class_def
    self.fields = {}
    self.id = instance_counter[0]
    instance_counter[0] += 1

  def to_string(self):
    return "<instance@" + str(self.id) + ": " + self.class_def.name.value + ">"

class MethodValue(Value):
  def __init__(self, instance, method_def):
    super().__init__('METHOD')
    self.method_def = method_def
    self.instance = instance

  def to_string(self):
    # TODO: better information here
    return "<method: " + self.method_def.name.value + ">"

class ConstructorValue(Value):
  def __init__(self, class_def):
    super().__init__('CONSTRUCTOR')
    self.class_def = class_def
  def to_string(self):
    return "<constructor: " + self.class_def.name.value + ">"
    
# Like a value except for executables

class StatusWrapper:
  def __init__(self, type, arg):
    self.type = type # RETURN, CONTINUE, BREAK, EXCEPTION
    self.arg = arg # a Value for RETURN, or a StackTrace for EXCEPTION

def new_error_status(token, error):
  return StatusWrapper('EXCEPTION', StackTrace(token, error))

def error_status_from_value(value):
  if not value.is_error: raise Exception("Incorrectly wrapped error value in error status")
  return StatusWrapper('EXCEPTION', value.stackTrace)