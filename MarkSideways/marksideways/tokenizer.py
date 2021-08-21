from .exceptions import ParserException

_KEYWORDS = [
  'for',
  'while',
  'if',
  'else',
  'break',
  'continue',
  'return',
  'till',
  'thru',
  'do',
  # Don't plan on implementing these for the hackathon, but go ahead and reserve them
  'switch',
  'case',
  'try',
  'catch',
  'finally',
  'throw',
]

class Token:
  def __init__(self, filename, value, line, col, token_type = None):
    self.filename = filename
    self.value = value
    self.line = line
    self.col = col
    self.token_type = token_type

class TokenStream:
  def __init__(self, filename, tokens):
    self.filename = filename
    self.tokens = tokens
    self.index = 0
    self.length = len(tokens)

  def eof(self):
    if self.length == 0:
      raise ParserException(Token(self.filename, '', 1, 1), "Unexpected EOF")
    raise ParserException(self.tokens[-1], "Unexpected EOF")

  def has_more(self):
    return self.index < self.length

  def peek(self):
    if self.index < self.length: return self.tokens[self.index]
    return None
  
  def peek_value(self):
    token = self.peek()
    if token != None: return token.value
    return None
  
  def pop(self):
    if self.index < self.length:
      token = self.tokens[self.index]
      self.index += 1
      return token
    self.eof()

  def pop_if_present(self, value):
    if self.peek_value() == value:
      self.index += 1
      return True
    return False
  
  def pop_expected(self, value):
    token = self.pop()
    if token.value != value:
      raise ParserException(token, "Expected '" + value + "' but found '" +  token.value + "' instead.")
    return token

  def is_next(self, value):
    return self.peek_value() == value


def _get_line_type(line):
  if line.startswith("#"): 
    if line.startswith("###"): return "H3"
    if line.startswith("##"): return "H2"
    if line.startswith("#"): return "H1"
    return "H+"

  if line.startswith("* `") or line.startswith("- `"):
    return "ARG"

  if line.rstrip() == "```": return "CODE"
  if line.strip() == "": return "BLANK"

  return "COMMENT"

def markdown_tokenize(filename, text):
  text = text.replace("\r\n", "\n").replace("\r", "\n").rstrip() + "\n"

  nest_level = 0
  lines = text.split('\n')
  in_code = False
  chunks = []
  i = 0
  while i < len(lines):
    line = lines[i]
    line_type = _get_line_type(line)
    if line_type == "H+":
      token = Token(filename, line[:30] + "...", i + 1, 1)
      raise ParserException(token, "Invalid header type. Must be limited to #, ##, and ###")

    if line_type == "BLANK" or line_type == "COMMENT":
      pass
    elif line_type[0] == "H":
      t = "title" if line_type == "H1" else ("class" if line_type == "H2" else "method")
      chunks.append({
        "type": t,
        "value": line[1:].lstrip('#').strip(),
        "token": Token(filename, line, i + 1, 1)
      })
    elif line_type == "ARG":
      # This is a bit of lazy parsing
      t = line[len("* `"):].split("`")
      token = Token(filename, t[0], i + 1, 3)
      if len(t) == 1: raise ParserException(token, "Invalid argument. Missing closing backtick")
      chunks.append({
        "type": "argument",
        "value": t[0],
        "token": token,
      })
    elif line_type == "CODE":
      cleanly_closed = False
      i += 1
      start_line = i
      while i < len(lines):
        line = lines[i]
        if line.strip() == "```":
          cleanly_closed = True
          end_line = i
          break
        i += 1
      if not cleanly_closed:
        raise ParserException(start_token, "Unclosed code block starting here!")
      code = '\n'.join(lines[start_line:end_line])
      chunks.append({
        "type": "code",
        "value": code,
        "line_offset": start_line,
      })
    else:
      raise Exception("Unknown line type: " + line_type)
    i += 1
  return chunks

def code_tokenize(filename, code):
  lines = []
  cols = []

  line = 1
  col = 1
  for c in code:
    lines.append(line)
    cols.append(col)
    if c == '\n':
      line += 1
      col = 1
    else:
      col += 1
  
  numbers = {}
  for i in range(10):
    numbers[str(i)] = True
  alphanums = {}
  for c in 'abcdefghijklmnopqrstuvwxyz0123456789_':
    alphanums[c.lower()] = True
    alphanums[c.upper()] = True
  multichar_tokens = {}
  for mc in '== != <= >= && || += -= *= /= %= ?? ** >> << &= |= ^= ++ --'.split():
    multichar_tokens[mc] = True
  keywords = {}
  for keyword in _KEYWORDS:
    keywords[keyword] = keyword

  tokens = []
  state = 'NORMAL'
  i = 0
  length = len(code)
  token_start = None
  token_type = None
  while i < length:
    c = code[i]
    if state == 'NORMAL':
      if c in (' ', '\n', '\t'):
        pass
      elif c in ('"', "'"):
        token_start = i
        state = 'STRING'
        token_type = c
      elif alphanums.get(c) != None:
        token_start = i
        state = 'WORD'
      elif i + 1 < length and multichar_tokens.get(c + code[i + 1]) != None:
        tokens.append(Token(filename, c + code[i + 1], lines[i], cols[i], 'PUNC'))
        i += 1
      else:
        tokens.append(Token(filename, c, lines[i], cols[i], 'PUNC'))
    elif state == 'STRING':
      if c == token_type:
        value = code[token_start:i + 1]
        tokens.append(Token(filename, value, lines[token_start], cols[token_start], 'STRING'))
        state = 'NORMAL'
      elif c == '\\':
        i += 1 # Check for whether the escape sequence is valid later
    elif state == 'WORD':
      if alphanums.get(c) == None:
        value = code[token_start:i]
        token_type = 'KEYWORD' if keywords.get(value) != None else ('NUMBER' if numbers.get(value[0]) != None else 'WORD')
        tokens.append(Token(filename, value, lines[token_start], cols[token_start], token_type))
        state = 'NORMAL'
        i -= 1
    i += 1
  
  if state != 'NORMAL':
    raise ParserException(Token(filename, code[token_start:token_start + 30] + '...', lines[token_start], cols[token_start], 'STRING'), "It looks like you have an unclosed string here.")
  
  # This is a little hacky but it consolidates floating point decimals into single tokens
  i = 0
  new_tokens = []
  length = len(tokens)
  while i < length:
    token = tokens[i]
    continuous_chain_size = 1

    if i + 1 < length and token.token_type == 'NUMBER' and tokens[i + 1].value == '.':
      if i + 2 < length and tokens[i + 2].token_type == 'NUMBER':
        continuous_chain_size = 3
      else:
        continuous_chain_size = 2
    elif token.value == '.' and i + 1 < length and tokens[i + 1].token_type == 'NUMBER':
      continuous_chain_size = 2
    
    if continuous_chain_size > 1:
      col = token.col
      for j in range(2, continuous_chain_size + 1):
        next_token = tokens[i + j - 1]
        if next_token.line == token.line and next_token.col == col + len(token.value):
          col = next_token.col
        else:
          continuous_chain_size = 1
          break

    if continuous_chain_size > 1:
      new_token_value = ''
      for j in range(continuous_chain_size):
        new_token_value += tokens[i + j]
      new_tokens.append(Token(filename, new_token_value, token.line, token.col, 'FLOAT'))
    else:
      new_tokens.append(token)

    i += continuous_chain_size

  return TokenStream(filename, new_tokens)
