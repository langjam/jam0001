import random
import time
from .values import *
from .builtinlibgame import get_game_lib

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
  
  gamelib = get_game_lib()

  def _floor(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_num(throw_token, args, 0)
    if err != None: return err
    n = args[0]
    if n.type == 'INT': return n
    if n.type == 'FLOAT': 
      return get_integer_value(int(n.value))
    return new_error_value(throw_token, "floor() can only accept number values.")

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
  
  def _unix_time(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0)
    if err != None: return err
    return FloatValue(time.time())


  def _game_create_window(throw_token, args):
    err = ensure_arg_count(throw_token, args, 4)
    if err != None: return err
    
    err = ensure_is_string(throw_token, args, 0)
    if err != None: return err

    for i in (1, 2, 3):
      err = ensure_is_num(throw_token, args, i)
      if err != None: return err

    title = args[0].value
    width = args[1].value
    height = args[2].value
    fps = args[3].value

    if fps < 30 or fps > 60: 
      return new_error_value("Invalid FPS: must be in the range of 30 to 60")

    err = gamelib['create_window'](throw_token, [title, width, height, fps])

  def _game_end_frame(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0)
    if err != None: return err
    gamelib['end_frame'](throw_token, args)
  
  def _game_fill_screen(throw_token, args):
    err = ensure_arg_count(throw_token, args, 3)
    if err != None: return err
    for i in range(3):
      err = ensure_is_num(throw_token, args, i)
      if err != None: return err
    gamelib['fill_screen'](throw_token, args)

  def _game_draw_rectangle(throw_token, args):
    err = ensure_arg_count(throw_token, args, 7)
    if err != None: return err
    for i in range(7):
      err = ensure_is_num(throw_token, args, i)
      if err != None: return err
    gamelib['draw_rectangle'](throw_token, args)

  def _game_is_quit(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0)
    if err != None: return err
    return gamelib['is_quit'](throw_token, args)

  lookup = {
    'floor': _floor,
    'game_create_window': _game_create_window,
    'game_draw_rectangle': _game_draw_rectangle,
    'game_end_frame': _game_end_frame,
    'game_fill_screen': _game_fill_screen,
    'game_is_quit': _game_is_quit,
    'parse_int': _parse_int,
    'print': _print,
    'random_float': _random_float,
    'sqrt': _sqrt,
    'unix_time': _unix_time,
  }

  output = {}
  for k in lookup.keys():
    output[k] = BuiltInFunction(k, lookup[k])
  return output

