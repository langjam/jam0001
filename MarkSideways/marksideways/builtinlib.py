import json
import random
import sys
import time
from .values import *
from .builtinlibgame import get_game_lib
from .builtinlibhttpserve import get_httpserve_lib
from .util import *

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
    return new_error_value(throw_token, "Invalid number of arguments. Expected " + expect + + " but received " + str(len(args)) + ".")

def ensure_is_bool(throw_token, args, arg_index):
  arg = args[arg_index]
  if arg.type != 'BOOL':
    return new_error_value(throw_token, "Invalid argument. Expected a boolean for argument #" + str(arg_index + 1))

def ensure_is_integer(throw_token, args, arg_index):
  arg = args[arg_index]
  if arg.type != 'INT':
    return new_error_value(throw_token, "Invalid argument. Expected an integer for argument #" + str(arg_index + 1))

def ensure_is_num(throw_token, args, arg_index):
  arg = args[arg_index]
  if arg.type != 'FLOAT' and arg.type != 'INT':
    return new_error_value(throw_token, "Invalid argument. Expected a number for argument #" + str(arg_index + 1))

def ensure_is_string(throw_token, args, arg_index):
  if args[arg_index].type != 'STRING':
    return new_error_value(throw_token, "Invalid argument. Expected a string for argument #" + str(arg_index + 1))

def ensure_is_function(throw_token, args, arg_index):
  if not args[arg_index].is_invocable:
    return new_error_value(throw_token, "Invalid argument. Expected an invocable function for argument #" + str(arg_index + 1))

def generate_builtins():
  
  gamelib = get_game_lib()
  httpservelib = get_httpserve_lib()

  def _assert(throw_token, args):
    err = ensure_arg_count(throw_token, args, 2)
    if err != None: return err
    err = ensure_is_bool(throw_token, args, 0)
    if err != None: return err
    
    if not args[0].value:
      return new_error_value(throw_token, "ASSERTION FAILED: " + args[1].to_string())
    return NULL_VALUE

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

  def _json_parse(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_string(throw_token, args, 0)
    if err != None: return err
    try:
      value = json.loads(args[0].value)
    except:
      err = sys.exc_info()[1]
      return new_error_value(throw_token, "Invalid JSON string. You have a syntax error on line " + str(err.lineno) + ", column " + str(err.colno) + ".")
    return convert_python_value(value)

  def _json_serialize(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1, 2)
    if err != None: return err
    if len(args) == 1:
      use_pretty_print = False
    else:
      err = ensure_is_bool(throw_token, args, 1)
      if err != None: return err
      use_pretty_print = args[1].value
    value = extract_py_value(args[0])
    if value == None: return new_error_value(throw_token, "Data contains a value that cannot be serialized to JSON")
    s = json.dumps(value) if not use_pretty_print else json.dumps(value, indent = 2)
    return StringValue(s)
      

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

  def _read_input(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0, 1)
    if err != None: return err
    if len(args) == 1:
      err = ensure_is_string(throw_token, args, 0)
      if err != None: return err
      prompt_str = args[0].value
      value = input(prompt_str)
    else:
      value = input('? ')
    return StringValue(value)

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

  def _game_get_events(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0)
    if err != None: return err
    return gamelib['get_events'](throw_token, args)

  def _game_is_key_pressed(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_string(throw_token, args, 0)
    if err != None: return err
    return gamelib['is_key_pressed'](throw_token, args)

  def _game_is_quit(throw_token, args):
    err = ensure_arg_count(throw_token, args, 0)
    if err != None: return err
    return gamelib['is_quit'](throw_token, args)

  def _game_set_title(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_string(throw_token, args, 0)
    if err != None: return err
    return gamelib['set_title'](throw_token, args)

  def _http_server_create_handler(throw_token, args):
    err = ensure_arg_count(throw_token, args, 3)
    if err != None: return err
    for i in (0, 1):
      err = ensure_is_string(throw_token, args, i)
      if err != None: return err
    err = ensure_is_function(throw_token, args, 2)
    if err != None: return err
    return httpservelib['create_handler'](throw_token, args[0].value, args[1].value, args[2])

  def _http_server_start(throw_token, args):
    err = ensure_arg_count(throw_token, args, 1)
    if err != None: return err
    err = ensure_is_integer(throw_token, args, 0)
    if err != None: return err
    port = args[0].value
    if port < 1 or port > 65535:
      return new_error_value(throw_token, "Invalid port number! Cannot use " + str(port) + ".")
    return httpservelib['start'](throw_token, port, get_scope_globals_hack())

  lookup = {
    'assert': _assert,
    'floor': _floor,
    'game_create_window': _game_create_window,
    'game_draw_rectangle': _game_draw_rectangle,
    'game_end_frame': _game_end_frame,
    'game_fill_screen': _game_fill_screen,
    'game_get_events': _game_get_events,
    'game_is_key_pressed': _game_is_key_pressed,
    'game_is_quit': _game_is_quit,
    'game_set_title': _game_set_title,
    'http_server_create_handler': _http_server_create_handler,
    'http_server_start': _http_server_start,
    'json_parse': _json_parse,
    'json_serialize': _json_serialize,
    'parse_int': _parse_int,
    'print': _print,
    'random_float': _random_float,
    'read_input': _read_input,
    'sqrt': _sqrt,
    'unix_time': _unix_time,
  }

  output = {}
  for k in lookup.keys():
    output[k] = BuiltInFunction(k, lookup[k])
  return output

