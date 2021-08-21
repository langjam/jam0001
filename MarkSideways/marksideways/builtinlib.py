import random
from .values import *

def ensure_arg_count(throw_token, args, min, max = None):
  if max == None: max = min
  expect = None
  
  if len(args) < min:
    expect = "at least " + str(min)
    if min == max: expect = str(min)
  if len(args) > max:
    expect = "at most " + str(max)
    if min == max: expect = str(min)
  
  if expect != None:
    return ErrorValue(throw_token, "Invalid number of arguments. Expected " + expect + + " but received " + str(len(args)) + ".")

def ensure_is_num(throw_token, args, arg_index):
  arg = args[arg_index]
  if arg.type != 'FLOAT' and arg.type != 'INT':
    return ErrorValue(throw_token, "Invalid argument. Expected a number for argument #" + str(arg_index + 1))

def ensure_is_string(throw_token, args, arg_index):
  if args[arg_index].type != 'STRING':
    return ErrorValue(throw_token, "Invalid argument. Expected a string for argument #" + str(arg_index + 1))

def generate_builtins():
  
  def _parse_int(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_string(throw_token, args, 0)
    if err != None: return err
    try:
      value = int(args[0].value)
      return get_integer_value(value)
    except:
      return NULL_VALUE

  def _print(throw_token, args):
    print(' '.join(map(lambda arg: arg.to_string(), args)))
    return NULL_VALUE

  def _random_float(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0)
    if err != None: return err
    return FloatValue(random.random())

  def _sqrt(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_num(throw_token, args, 0)
    if err != None: return err
    return FloatValue(args[0].value ** .5)
  
  lookup = {
    'parse_int': _parse_int,
    'print': _print,
    'random_float': _random_float,
    'sqrt': _sqrt,
  }

  output = {}
  for k in lookup.keys():
    output[k] = BuiltInFunction(k, lookup[k])
  return output

