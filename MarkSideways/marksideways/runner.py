from .exceptions import ParserException
from .tokenizer import markdown_tokenize, code_tokenize, Token
from .nodes import *

class Runner:
  def __init__(self, filename, text):
    self.filename = filename
    self.text = text

  def run(self):
    documentItems = markdown_tokenize(self.filename, self.text)
    if len(documentItems) == 0:
      raise ParserException(Token(self.filename, "", 1, 1), "Unexpected EOF. Document seems to be empty.")
    if documentItems[0]['type'] != "title":
      raise ParserException(documentItems[0].token, "Document title/program name must come first")
    document = _parse_markdown_structure(documentItems)

    parse_document(document)
    
def _parse_markdown_structure(items):
  root = None
  active_class = None
  active_method = None
  for item in items:
    if item['type'] == "title":
      if root != None:
        raise ParserException(item['token'], "Document title can only appear once")
      root = ProgramRoot(item['token'], item['value'])

    elif item['type'] == "class":
      class_def = ClassDefinition(item['token'], item['value'])
      active_class = class_def
      root.add_class(class_def)
      active_method = None
    
    elif item['type'] == "method":
      method_def = MethodDefinition(item['token'], item['value'])
      if class_def == None:
        root.methods.append(method_def)
      else:
        class_def.add_method(method_def)
      active_method = method_def
    
    elif item['type'] == "argument":
      arg_name = item['value']
      arg_token = item['token']
      if active_method != None:
        if len(active_method.code_lines) > 0:
          raise ParserException(arg_token, "Cannot declare arguments in a method after code has already been defined")
        active_method.add_argument(arg_token, arg_name)
      elif active_class != None:
        active_class.add_argument(arg_token, arg_name)
      else:
        pass # Just ignore it! It is a regular comment in a bulleted list
    
    elif item['type'] == "code":
      if active_method != None:
        active_method.add_code(item['value'], item['line_offset'])
      elif active_class != None:
        active_class.add_code(item['value'], item['line_offset'])
      else:
        root.add_code(item['value'], item['line_offset'])
  
  return root

def parse_document(document):
  parse_code_lines(document, document.code_lines)
  for method_def in document.method_order:
    parse_method(method_def)
  for class_def in document.classes:
    parse_class(class_def)

def parse_method(method_def):
  method_def.code = parse_code_lines(method_def.code_lines)

def parse_class(class_def):
  class_def.code = parse_code_lines(class_def.code_lines)
  for method_def in class_def.method_order:
    parse_method(method_def)

def parse_code_lines(parent, lines):
  filename = parent.first_token.filename
  code = '\n'.join(parent.code_lines) + '\n'
  tokens = code_tokenize(filename, code)

  print('\n'.join(map(lambda token: token.value, tokens.tokens)))

  raise Exception("Not implemented: You left off here!")
  #parent.code = parse_code(tokens)
