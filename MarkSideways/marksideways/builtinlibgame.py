import os
import sys
import time

from .values import *
from .util import *

# Suppress the welcome message STDOUT spam when importing pygame.
# This is the official method which sometimes does not work:
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = 'hide'

# This is the unofficial brute force version:
sys.stdout = open(os.devnull, 'w')
try:
  import pygame
  failure = False
except:
  failure = True
# Re-enable STDOUT
sys.stdout = sys.__stdout__


def ensure_color(throw_token, r, g, b):
  if not is_color(r.value, g.value, b.value):
    return new_error_value(throw_token, "Invalid color. Color values must be integers between 0 and 255")
  return None

def is_color(r, g, b):
  for color in (r, g, b):
    if int(color) != color: return False
    if color < 0 or color > 255: return False
  return True

_PY_KEY_LOOKUP = {}
if not failure:
  _PY_KEY_LOOKUP = {
    pygame.K_UP: 'up',
    pygame.K_DOWN: 'down',
    pygame.K_LEFT: 'left',
    pygame.K_RIGHT: 'right',
    pygame.K_SPACE: 'space',
    pygame.K_RETURN: 'enter',
    pygame.K_ESCAPE: 'escape',
    pygame.K_LCTRL: 'ctrl',
    pygame.K_RCTRL: 'ctrl',
    pygame.K_LSHIFT: 'shift',
    pygame.K_RSHIFT: 'shift',
    pygame.K_LALT: 'alt',
    pygame.K_RALT: 'alt',
  }
  for i in range(26):
    _PY_KEY_LOOKUP[pygame.K_a + i] = chr(ord('a') + i)
  for i in range(10):
    _PY_KEY_LOOKUP[pygame.K_0 + i] = 'num' + str(i)
  for i in range(12):
    _PY_KEY_LOOKUP[pygame.K_F1 + i] = 'f' + str(i + 1)

_PY_VALID_KEYS = {}
for k in _PY_KEY_LOOKUP.values():
  _PY_VALID_KEYS[k] = True

def get_game_lib():
  vars = {
    'screen': None,
    'frame_begin': 0.0,
    'events': [],
    'fps': 30,
    'is_quit': False,
    'keys_pressed': {},
  }

  def ERR(throw_token, args):
    return new_error_value(throw_token, "Game library not available. Please run `pip install pygame` to enable this in the interpreter runtime.")

  def _create_window(throw_token, args):
    pygame.init()
    vars['screen'] = pygame.display.set_mode((args[1], args[2]))
    vars['frame_begin'] = time.time()
    vars['fps'] = args[3]
    pygame.display.set_caption(args[0])
    icon = pygame.image.load(os.path.join(get_marksideways_dir(), 'marksideways', 'icon.png')).convert_alpha()
    pygame.display.set_icon(icon)

  def _end_frame(throw_token, args):
    pygame.display.flip()
    events = []
    for e in pygame.event.get():
      if e.type == pygame.QUIT:
        vars['is_quit'] = True
      elif e.type == pygame.KEYUP or e.type == pygame.KEYDOWN:
        down = e.type == pygame.KEYDOWN
        key = _PY_KEY_LOOKUP.get(e.key)
        if key != None:
          if vars['keys_pressed'].get(key, False) != down:
            vars['keys_pressed'][key] = down
            event_name = key + ':' + ('press' if down else 'release')
            events.append(event_name)
            if event_name == 'w:press' and vars['keys_pressed'].get('ctrl') != None:
              vars['is_quit'] = True
            if event_name == 'f4:press' and vars['keys_pressed'].get('alt') != None:
              vars['is_quit'] = True
          
          
    vars['events'] = events
    
    diff = time.time() - vars['frame_begin']
    delay = 1.0 / vars['fps'] - diff
    if delay > 0:
      time.sleep(delay)
    vars['frame_begin'] = time.time()

  def _fill_screen(throw_token, args):
    r = args[0]
    g = args[1]
    b = args[2]
    err = ensure_color(throw_token, r, g, b)
    if err != None: return err
    vars['screen'].fill((r.value, g.value, b.value))
    
  def _draw_rectangle(throw_token, args):
    r = args[4]
    g = args[5]
    b = args[6]
    err = ensure_color(throw_token, r, g, b)
    if err != None: return err

    pygame.draw.rect(vars['screen'], (r.value, g.value, b.value), pygame.Rect(args[0].value, args[1].value, args[2].value, args[3].value))

  def _get_events(throw_token, args):
    return ArrayValue(list(map(lambda ev_name: StringValue(ev_name), vars['events'])))

  def _is_key_pressed(throw_token, args):
    key = args[0].value
    if not _PY_VALID_KEYS.get(key, False):
      return new_error_value(throw_token, "'" + key + "' is not a valid recognized key.")
    return get_bool_value(vars['keys_pressed'].get(key, False))

  def _is_quit(throw_token, args):
    return TRUE_VALUE if vars['is_quit'] else FALSE_VALUE

  def _set_title(throw_token, args):
    title = args[0].value.strip()
    if len(title) == 0:
      return new_error_value(throw_token, "Cannot set a blank title.")
    pygame.display.set_caption(title)
    return NULL_VALUE

  return {
    'create_window': ERR if failure else _create_window,
    'end_frame': ERR if failure else _end_frame,
    'fill_screen': ERR if failure else _fill_screen,
    'draw_rectangle': ERR if failure else _draw_rectangle,
    'get_events': ERR if failure else _get_events,
    'is_key_pressed': ERR if failure else _is_key_pressed,
    'is_quit': ERR if failure else _is_quit,
    'set_title': ERR if failure else _set_title,
  }
