from .exceptions import *

ALPHA_NUM = {}
for c in '0123456789_abcdefghijklmnopqrstuvwxyz':
  ALPHA_NUM[c.lower()] = c.lower()
  ALPHA_NUM[c.upper()] = c.lower()

def canonicalize_identifier(value):
  sb = []
  for c in value:
    sb.append(ALPHA_NUM.get(c, ''))
  return ''.join(sb)

def list_to_lookup(items):
  output = {}
  for item in items:
    output[str(item)] = True
  return output

_ESCAPE_SEQUENCES = {
  'n': '\n',
  'r': '\r',
  't': '\t',
  '"': '"',
  "'": "'",
  '\\': '\\',
  '0': '\0',
  # TODO: support unicode - the most hackathon-y comment imaginable
}

def string_literal_to_value(throw_token, value):
  sb = []
  i = 0
  length = len(value)
  while i < length:
    c = value[i]
    if c == '\\':
      if i + 1 >= length:
        raise ParserException(throw_token, "String literal ends with an escape sequence opening backslash.")
      c = _ESCAPE_SEQUENCES.get(value[i + 1])
      i += 1
      if c == None:
        raise ParserException(throw_token, "Invalid escape sequence: \\" + value[i + 1])
    sb.append(c)
    i += 1

  return ''.join(sb)

def merge_dictionaries(a, b):
  output = {}
  for d in [a, b]:
    for k in d.keys():
      output[k] = d[k]
  return output

_ms_dir = None
def set_marksideways_dir(dir):
  global _ms_dir
  _ms_dir = dir

def get_marksideways_dir():
  return _ms_dir

# It's the last evening of a hackathon so I deserve a break!
# This is a way to just save the interpreter's scope.globals value in a centralized way.
# This is accessed by the HTTP server when it runs handlers.
_sgh = None
def set_scope_globals_hack(g):
  global _sgh
  _sgh = g
def get_scope_globals_hack():
  return _sgh