from .tokenizer import markdown_tokenize, Token
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
    document = self._parse_markdown_structure(documentItems)
    

  def _parse_markdown_structure(self, items):
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
      
      elif item['type'] == "method":
        method_def = MethodDefinition(item['token'], item['value'])
        if class_def == None:
          root.methods.append(method_def)
        else:
          class_def.add_method(method_def)
        active_method = method_def
      
      elif item['type'] == "argument":
        if active_method == None:
          pass # Just ignore it! It is a bonafied comment
        elif len(active_method.code_chunks) > 0:
          raise ParserException(item['token'], "Cannot declare arguments in a method after code has already been defined")
        else:
          active_method.add_argument(item['token'], item['value'])
      
      elif item['type'] == "code":
        if active_method != None:
          active_method.add_code(item['value'], item['line_offset'])
        else:
          root.add_code(item['value'], item['line_offset'])
