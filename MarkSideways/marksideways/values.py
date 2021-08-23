# TODO: all the to_string methods for complex types need to have a cycle check

from .exceptions import *

class Value:
  def __init__(self, _type):
    self.type = _type
    self.is_error = False
    self.is_invocable = False

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

class DictionaryValue(Value):
  def __init__(self):
    super().__init__('DICTIONARY')
    self.keys = []
    self.values = []
    self.lookup = {}
    self.size = 0
    self.fields = {}
  def set_item(self, throw_token, key, value):
    nkey = self.get_native_key(key)
    if nkey == None: return self.gen_error_key(throw_token, key)
    existing_index = self.lookup.get(nkey)
    if existing_index != None:
      self.values[existing_index] = value
      return True
    self.lookup[nkey] = len(self.keys)
    self.keys.append(key)
    self.values.append(value)
    self.size += 1
    return False
  def get_item(self, throw_token, key):
    nkey = self.get_native_key(key)
    if nkey == None: return self.gen_error_key(throw_token, key)
    index = self.lookup.get(nkey)
    if index == None: return None
    return self.values[index]
  def gen_error_key(self, throw_token, key):
    return new_error_value(throw_token, "This is not a valid type that can be used as a key: " + key.type)
  def remove_item(self, throw_token, key):
    nkey = self.get_native_key(key)
    if nkey == None: return self.gen_error_key(throw_token, key)
    index = self.lookup.get(nkey)
    if index == None: return new_error_value(throw_token, "The key " + key.to_string() + " does not exist in this dictionary.")
    last_index = self.size - 1
    self.lookup.pop(nkey)
    if index == last_index:
      self.keys.pop()
      self.values.pop()
    else:
      swap_key = self.keys.pop()
      swap_value = self.values.pop()
      self.lookup[self.get_native_key(swap_key)] = index
      self.keys[index] = swap_key
      self.values[index] = swap_value
    self.size -= 1
    return NULL_VALUE
    
  def get_native_key(self, value):
    if value.type == 'INT': return 'i:' + str(value.value)
    if value.type == 'STRING': return 's:' + value.value
    if value.type == 'INSTANCE': return 'o:' + str(value.id)
    if value.type == 'BOOL': return 'b:' + ('1' if value.value else '0')
    if value.type in ('ARRAY', 'DICTIONARY', 'NULL', 'FLOAT'): return None
    return '*:' + value.to_string()
  def _builtin_get(self, throw_token, args):
    if len(args) == 1:
      default_value = NULL_VALUE
    elif len(args) == 2:
      default_value = args[1]
    else:
      return new_error_value(throw_token, "Incorrect number of args to dictionary.get(). Requires 1 or 2.")
    key = args[0]
    output = self.get_item(throw_token, key)
    if output == None:
      return default_value
    return output
  def _builtin_keys(self, throw_token, args):
    if len(args) != 0: return new_error_value(throw_token, "Dictionary.keys() does not take in any arguments.")
    return ArrayValue(self.keys[:])
  def _builtin_values(self, throw_token, args):
    if len(args) != 0: return new_error_value(throw_token, "Dictionary.values() does not take in any arguments.")
    return ArrayValue(self.values[:])
  def _builtin_remove(self, throw_token, args):
    if len(args) != 1: return new_error_value(throw_token, "Dictionary.remove() requires a single key argument.")
    return self.remove_item(throw_token, args[0])
  def get_field(self, name):
    output = self.fields.get(name)
    if output == None:
      if name == 'get': output = BuiltInFunction('dictionary.get', self._builtin_get)
      elif name == 'keys': output = BuiltInFunction('dictionary.keys', self._builtin_keys)
      elif name == 'values': output = BuiltInFunction('dictionary.values', self._builtin_values)
      elif name == 'remove': output = BuiltInFunction('dictionary.remove', self._builtin_remove)
      self.fields[name] = output
    return output
  def to_string(self):
    sb = ['{']
    for i in range(len(self.keys)):
      if i > 0: sb.append(', ')
      sb.append(self.keys[i].to_string())
      sb.append(': ')
      sb.append(self.values[i].to_string())
    sb.append(' }')
    return ''.join(sb)


class BuiltInFunction(Value):
  def __init__(self, id, handler):
    super().__init__('BUILTIN_FUNCTION')
    self.handler = handler
    self.is_invocable = True
  def to_string(self):
    return "<Built-in function: " + self.id + ">"

class FunctionValue(Value):
  def __init__(self, method_def):
    super().__init__('FUNCTION')
    self.method_def = method_def
    self.is_invocable = True
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
    self.is_invocable = True

  def to_string(self):
    # TODO: better information here
    return "<method: " + self.method_def.name.value + ">"

class ConstructorValue(Value):
  def __init__(self, class_def):
    super().__init__('CONSTRUCTOR')
    self.class_def = class_def
    self.is_invocable = True
  def to_string(self):
    return "<constructor: " + self.class_def.name.value + ">"
    
# Like a value except for executables

class StatusWrapper:
  def __init__(self, type, arg):
    self.type = type # RETURN, CONTINUE, BREAK, EXCEPTION
    self.arg = arg # a Value for RETURN, or a StackTrace for EXCEPTION

PY_BOOL_TYPE = type(True)
PY_STR_TYPE = type('')
PY_INT_TYPE = type(1)
PY_FLOAT_TYPE = type(1.5)
PY_LIST_TYPE = type([])
PY_DICT_TYPE = type({})

# this is only used from the JSON parser. If you need to use it for something else
# then it needs a few updates and additions (no throw token, only a few types supported)
def convert_python_value(value):
  if value == None: return NULL_VALUE
  t = type(value)
  if t == PY_BOOL_TYPE: return get_bool_value(value)
  if t == PY_STR_TYPE: return StringValue(value)
  if t == PY_INT_TYPE: return get_integer_value(value)
  if t == PY_FLOAT_TYPE: return FloatValue(value)
  if t == PY_LIST_TYPE:
    items = map(convert_python_value, value)
    return ArrayValue(list(items))
  if t == PY_DICT_TYPE:
    output = DictionaryValue()
    for key in value.keys():
      wrapped_key = convert_python_value(key)
      wrapped_value = convert_python_value(value[key])
      output.set_item(None, wrapped_key, wrapped_value)
    return output
  
  return NULL_VALUE

def extract_py_value(value):
  t = value.type
  if t == 'NULL': return None
  if t in ('BOOL', 'INT', 'FLOAT', 'STRING'):  return value.value
  if t == 'ARRAY':
    return list(map(extract_py_value, value.value))
  if t == 'DICTIONARY':
    output = {}
    keys = value.keys
    values = value.values
    for i in range(len(keys)):
      if keys[i].type != 'STRING': return None
      key = extract_py_value(keys[i])
      output[key] = extract_py_value(values[i])
    return output
  return None


def new_error_status(token, error):
  return StatusWrapper('EXCEPTION', StackTrace(token, error))

def error_status_from_value(value):
  if not value.is_error: raise Exception("Incorrectly wrapped error value in error status")
  return StatusWrapper('EXCEPTION', value.stackTrace)
