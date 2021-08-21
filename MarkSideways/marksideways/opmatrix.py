from .values import *

_value_equal = lambda t, a, b: (TRUE_VALUE if a.value == b.value else FALSE_VALUE)

OP_MATRIX = {
  'INT+INT': lambda t, a, b: get_integer_value(a.value + b.value),
  'INT-INT': lambda t, a, b: get_integer_value(a.value - b.value),
  'INT*INT': lambda t, a, b: get_integer_value(a.value * b.value),
  'INT/INT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else get_integer_value(a.value // b.value)),
  'INT%INT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else get_integer_value(int(a.value % b.value))),
  'INT**INT': lambda t, a, b: get_integer_value(int(a.value ** b.value)),
  'INT<INT': lambda t, a, b: get_bool_value(a.value < b.value),
  'INT>INT': lambda t, a, b: get_bool_value(a.value > b.value),
  'INT<=INT': lambda t, a, b: get_bool_value(a.value <= b.value),
  'INT>=INT': lambda t, a, b: get_bool_value(a.value >= b.value),
  'INT==INT': _value_equal,
  'FLOAT+INT': lambda t, a, b: FloatValue(a.value + b.value),
  'FLOAT-INT': lambda t, a, b: FloatValue(a.value - b.value),
  'FLOAT*INT': lambda t, a, b: FloatValue(a.value * b.value),
  'FLOAT/INT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value / b.value)),
  'FLOAT%INT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value % b.value)),
  'FLOAT**INT': lambda t, a, b: FloatValue(float(a.value ** b.value)),
  'FLOAT<INT': lambda t, a, b: get_bool_value(a.value < b.value),
  'FLOAT>INT': lambda t, a, b: get_bool_value(a.value > b.value),
  'FLOAT<=INT': lambda t, a, b: get_bool_value(a.value <= b.value),
  'FLOAT>=INT': lambda t, a, b: get_bool_value(a.value >= b.value),
  'FLOAT==INT': _value_equal,
  'INT+FLOAT': lambda t, a, b: FloatValue(a.value + b.value),
  'INT-FLOAT': lambda t, a, b: FloatValue(a.value - b.value),
  'INT*FLOAT': lambda t, a, b: FloatValue(a.value * b.value),
  'INT/FLOAT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value / b.value)),
  'INT%FLOAT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value % b.value)),
  'INT**FLOAT': lambda t, a, b: FloatValue(float(a.value ** b.value)),
  'INT<FLOAT': lambda t, a, b: get_bool_value(a.value < b.value),
  'INT>FLOAT': lambda t, a, b: get_bool_value(a.value > b.value),
  'INT<=FLOAT': lambda t, a, b: get_bool_value(a.value <= b.value),
  'INT>=FLOAT': lambda t, a, b: get_bool_value(a.value >= b.value),
  'INT==FLOAT': _value_equal,
  'FLOAT+FLOAT': lambda t, a, b: FloatValue(a.value + b.value),
  'FLOAT-FLOAT': lambda t, a, b: FloatValue(a.value - b.value),
  'FLOAT*FLOAT': lambda t, a, b: FloatValue(a.value * b.value),
  'FLOAT/FLOAT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value / b.value)),
  'FLOAT%FLOAT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value % b.value)),
  'FLOAT**FLOAT': lambda t, a, b: FloatValue(float(a.value ** b.value)),
  'FLOAT<FLOAT': lambda t, a, b: get_bool_value(a.value < b.value),
  'FLOAT>FLOAT': lambda t, a, b: get_bool_value(a.value > b.value),
  'FLOAT<=FLOAT': lambda t, a, b: get_bool_value(a.value <= b.value),
  'FLOAT>=FLOAT': lambda t, a, b: get_bool_value(a.value >= b.value),
  'FLOAT==FLOAT': _value_equal,
  'BOOL==BOOL': _value_equal,
  'STRING==STRING': _value_equal,
  'ARRAY==ARRAY': _value_equal,
  'DICTIONARY==DICTIONARY': _value_equal,
  'CLASS==CLASS': lambda t, a, b: get_bool_value(a.class_def == b.class_def),
  'CONSTRUCTOR==CONSTRUCTOR': lambda t, a, b: get_bool_value(a.class_def == b.class_def),
  'FUNCTION==FUNCTION': lambda t, a, b: get_bool_value(a.method_def == b.method_def),
  'METHOD==METHOD': lambda t, a, b: get_bool_value(a.method_def == b.method_def and a.instance == b.instance),
  'INSTANCE==INSTANCE': lambda t, a, b: get_bool_value(a.instance == b.instance),
  'NULL==NULL': lambda t, a, b: TRUE_VALUE,
  'BUILTIN_FUNCTION==BUILTIN_FUNCTION': lambda t, a, b: get_bool_value(a.handler == b.handler),
  
  # NOTE: these are short-circuiting ops which is handled in the OpChain's .run() method 
  # specifically. All short-circuiting cases are handled before it gets to this point, so
  # this is okay.
  'BOOL&&BOOL': lambda t, a, b: get_bool_value(a.value and b.value),
  'BOOL||BOOL': lambda t, a, b: get_bool_value(a.value or b.value),
}

def perform_op(throw_token, left, op, right):
  if op == '!=':
    output = perform_op(throw_token, left, '==', right)
    return FALSE_VALUE if output.value else TRUE_VALUE
  id = left.type + op + right.type
  handler = OP_MATRIX.get(id)
  if handler == None:
    if op == '==': return FALSE_VALUE
    if op == '+' and (left.type == 'STRING' or right.type == 'STRING'):
      return StringValue(left.to_string() + right.to_string())
    
    # The short-circuiting of the left side being null is already handled directly
    # in the OpChain.run() method. If it gets to this point, it means that the left
    # side was null.
    if op == '??':
      return right

    if left.type == 'NULL': new_error_value(throw_token, "Null reference error: the left side of this op was null")
    if right.type == 'NULL': new_error_value(throw_token, "Null reference error: the right side of this op was null")
    return new_error_value(throw_token, "The op " + op + " is not defined for the types " + left.type + " and " + right.type + ".")
  return handler(throw_token, left, right)
