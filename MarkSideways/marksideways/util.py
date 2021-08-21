def canonicalize_identifier(value):
  sb = []
  a = ord('a')
  z = ord('z')
  n0 = ord('0')
  n9 = ord('9')
  for c in value.lower():
    ord_c = ord(c)
    if a <= ord_c <= z:
      sb.append(c)
    elif n0 <= ord_c <= n9:
      sb.append(c)
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
    
      