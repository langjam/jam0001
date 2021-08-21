import os
import sys
import time

from .values import *

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

def get_game_lib():
  vars = {
    'screen': None,
    'frame_begin': 0.0,
    'events': [],
    'fps': 30,
    'is_quit': False,
  }

  def ERR(throw_token, args):
    return new_error_value(throw_token, "Game library not available. Please run `pip install pygame` to enable this in the interpreter runtime.")

  def _create_window(throw_token, args):
    pygame.init()
    vars['screen'] = pygame.display.set_mode((args[1], args[2]))
    vars['frame_begin'] = time.time()
    vars['fps'] = args[3]
    pygame.display.set_caption(args[0])

  def _end_frame(throw_token, args):
    pygame.display.flip()
    events = []
    for e in pygame.event.get():
      if e.type == pygame.QUIT:
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

  def _is_quit(throw_token, args):
    return TRUE_VALUE if vars['is_quit'] else FALSE_VALUE

  return {
    'create_window': ERR if failure else _create_window,
    'end_frame': ERR if failure else _end_frame,
    'fill_screen': ERR if failure else _fill_screen,
    'draw_rectangle': ERR if failure else _draw_rectangle,
    'is_quit': ERR if failure else _is_quit,
  }
