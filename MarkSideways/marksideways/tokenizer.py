class Token:
  def __init__(self, filename, value, line, col):
    self.filename = filename
    self.value = value
    self.line = line
    self.col = col

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
      code = '\n'.join(lines[start_line + 1:end_line])
      chunks.append({
        "type": "code",
        "value": code,
        "line_offset": start_line,
      })
    else:
      raise Exception("Unknown line type: " + line_type)
    i += 1
  return chunks
