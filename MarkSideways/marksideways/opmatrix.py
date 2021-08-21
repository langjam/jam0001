from .values import *

OP_MATRIX = {
  'INT+INT': lambda t, a, b: get_integer_value(a.value + b.value),
  'INT-INT': lambda t, a, b: get_integer_value(a.value - b.value),
  'INT*INT': lambda t, a, b: get_integer_value(a.value * b.value),
  'INT/INT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else get_integer_value(a.value // b.value)),
  'INT<INT': lambda t, a, b: get_bool_value(a.value < b.value),
  'INT>INT': lambda t, a, b: get_bool_value(a.value > b.value),
  'INT<=INT': lambda t, a, b: get_bool_value(a.value < b.value),
  'INT>=INT': lambda t, a, b: get_bool_value(a.value > b.value),
  'FLOAT+INT': lambda t, a, b: FloatValue(a.value + b.value),
  'FLOAT-INT': lambda t, a, b: FloatValue(a.value - b.value),
  'FLOAT*INT': lambda t, a, b: FloatValue(a.value * b.value),
  'FLOAT/INT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value / b.value)),
  'FLOAT<INT': lambda t, a, b: get_bool_value(a.value < b.value),
  'FLOAT>INT': lambda t, a, b: get_bool_value(a.value > b.value),
  'FLOAT<=INT': lambda t, a, b: get_bool_value(a.value < b.value),
  'FLOAT>=INT': lambda t, a, b: get_bool_value(a.value > b.value),
  'INT+FLOAT': lambda t, a, b: FloatValue(a.value + b.value),
  'INT-FLOAT': lambda t, a, b: FloatValue(a.value - b.value),
  'INT*FLOAT': lambda t, a, b: FloatValue(a.value * b.value),
  'INT/FLOAT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value / b.value)),
  'INT<FLOAT': lambda t, a, b: get_bool_value(a.value < b.value),
  'INT>FLOAT': lambda t, a, b: get_bool_value(a.value > b.value),
  'INT<=FLOAT': lambda t, a, b: get_bool_value(a.value < b.value),
  'INT>=FLOAT': lambda t, a, b: get_bool_value(a.value > b.value),
  'FLOAT+FLOAT': lambda t, a, b: FloatValue(a.value + b.value),
  'FLOAT-FLOAT': lambda t, a, b: FloatValue(a.value - b.value),
  'FLOAT*FLOAT': lambda t, a, b: FloatValue(a.value * b.value),
  'FLOAT/FLOAT': lambda t, a, b: (new_error_value(t, "Division by zero has occurred!") if b == 0 else FloatValue(a.value / b.value)),
  'FLOAT<FLOAT': lambda t, a, b: get_bool_value(a.value < b.value),
  'FLOAT>FLOAT': lambda t, a, b: get_bool_value(a.value > b.value),
  'FLOAT<=FLOAT': lambda t, a, b: get_bool_value(a.value < b.value),
  'FLOAT>=FLOAT': lambda t, a, b: get_bool_value(a.value > b.value),
}

def perform_op(throw_token, left, op, right):
  id = left.type + op + right.type
  handler = OP_MATRIX.get(id)
  if handler == None:
    if op == '+' and (left.type == 'STRING' or right.type == 'STRING'):
      if left.value == '': return right
      if right.value == '': return left
      return StringValue(left.to_string() + right.to_string())
    if left.type == 'NULL': new_error_value(throw_token, "Null reference error: the left side of this op was null")
    if right.type == 'NULL': new_error_value(throw_token, "Null reference error: the right side of this op was null")
    return new_error_value(throw_token, "The op " + op + " is not defined for the types " + left.type + " and " + right.type + ".")
  return handler(throw_token, left, right)
