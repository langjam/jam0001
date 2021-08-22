from .exceptions import ParserException
from .nodes import *
from .util import *

def parse_code(tokens):
  executables = []
  while tokens.has_more():
    ex = parse_executable(tokens, True, True)
    executables.append(ex)
  return executables

_ASSIGN_OPS = {}
for ao in [
    '=',
    '+=', '-=', '*=', '/=', '%=',
    '&=', '|=', '^=',
  ]:
  _ASSIGN_OPS[ao] = True

def parse_executable(tokens, allow_complex = True, include_semicolon = True):
  next = tokens.peek()

  if next.token_type == 'KEYWORD':
    next_value = next.value
    
    if not allow_complex:
      raise ParserException(next, "Unexpected token: '" + next_value + "'")

    if next_value == 'if': return parse_if_statement(tokens)
    if next_value == 'while': return parse_while_loop(tokens)
    if next_value == 'for': return parse_for_loop(tokens)
    if next_value == 'do': return parse_do_while_loop(tokens)
    if next_value == 'return': return parse_return_statement(tokens)
    if next_value == 'break': return parse_break_statement(tokens)
    if next_value == 'continue': return parse_continue_statement(tokens)
  
  expr = parse_expression(tokens)
  if _ASSIGN_OPS.get(tokens.peek_value()) != None:
    op = tokens.pop()
    assigned_expression = parse_expression(tokens)
    output = AssignStatement(expr, op, assigned_expression)
  else:
    if isinstance(expr, FunctionInvocation) or isinstance(expr, InlineIncrement):
      output = ExpressionAsExecutable(expr)
    else:
      raise ParserException(expr.first_token, "This expression does nothing. Did you forget an assignment?")

  if include_semicolon:
    tokens.pop_expected(';')

  return output

def parse_code_block(tokens):
  tokens.pop_expected('{')
  output = []
  while not tokens.pop_if_present('}'):
    line = parse_executable(tokens, True, True)
    output.append(line)
  return output

def parse_break_statement(tokens):
  token = tokens.pop_expected('break')
  tokens.pop_expected(';')
  return BreakStatement(token)

def parse_continue_statement(tokens):
  token = tokens.pop_expected('continue')
  tokens.pop_expected(';')
  return ContinueStatement(token)

def parse_if_statement(tokens):
  if_token = tokens.pop_expected('if')
  condition = parse_expression(tokens)
  code = parse_code_block(tokens)
  else_code = []
  if tokens.is_next('else'):
    tokens.pop_expected('else')
    if tokens.is_next('if'):
      else_code = [parse_if_statement(tokens)]
    else:
      else_code = parse_code_block(tokens)
  return IfStatement(if_token, condition, code, else_code)

def parse_return_statement(tokens):
  return_token = tokens.pop_expected('return')
  if tokens.pop_if_present(';'):
    return ReturnStatement(return_token, None)
  expression = parse_expression(tokens)
  tokens.pop_expected(';')
  return ReturnStatement(return_token, expression)

def parse_for_loop(tokens):
  for_token = tokens.pop_expected('for')
  variable = tokens.pop()
  if variable.token_type != 'WORD': raise ParserException(variable, "Expected a valid variable name but found '" + variable.value + "'.")
  tokens.pop_expected('=')
  start = parse_expression(tokens)
  loop_op = tokens.peek()
  if loop_op.value not in ('thru', 'till'): tokens.pop_expected('till') # intentionally throw
  tokens.pop()
  end = parse_expression(tokens)
  code = parse_code_block(tokens)
  return ForLoop(for_token, variable, loop_op, start, end, code)

def parse_while_loop(tokens):
  while_token = tokens.pop_expected('while')
  condition = parse_expression(tokens)
  code = parse_code_block(tokens)
  return WhileLoop(while_token, condition, code)

def parse_do_while_loop(tokens):
  do_token = tokens.pop_expected('do')
  code = parse_code_block(tokens)
  tokens.pop_expected('while')
  condition = parse_expression(tokens)
  tokens.pop_expected(';')
  return DoWhileLoop(do_token, code, condition)

class OpChainParser:
  def __init__(self, ops, next_parser_func):
    self.ops = list_to_lookup(ops)
    self.next_parser_func = next_parser_func
  
  def parse(self, tokens):
    expr = self.next_parser_func(tokens)
    next_value = tokens.peek_value()
    if self.ops.get(next_value) != None:
      expressions = [expr]
      ops = []
      while self.ops.get(next_value) != None:
        ops.append(tokens.pop())
        expressions.append(self.next_parser_func(tokens))
        next_value =  tokens.peek_value()
      expr = OpChain(expressions, ops)
    return expr

def parse_expression(tokens):
  return parse_ternary(tokens)

def parse_unaries(tokens):
  next_value = tokens.peek_value()
  if next_value in ('~', '!', '-', '++', '--'):
    op = tokens.pop()
    expr = parse_unaries(tokens)
    if next_value == '++' or next_value == '--':
      return InlineIncrement(op, op, expr, True, next_value == '++')
    return UnaryPrefix(op, expr)
  expr = parse_exponents(tokens)
  next_value = tokens.peek_value()
  if next_value in ('++', '--'):
    op = tokens.pop()
    return InlineIncrement(expr.first_token, op, expr, False, next_value == '++')
  return expr

def parse_entity_with_suffix_chains(tokens):
  expr = parse_entity(tokens)
  check_suffixes = True
  while check_suffixes:
    next_value = tokens.peek_value()
    if next_value == '.':
      dot = tokens.pop()
      field_name = tokens.pop()
      if field_name.token_type != 'WORD': raise ParserException(field_name, "Expected a valid field name but found '" + field_name.value + "'.")
      expr = DotField(expr, dot, field_name)
    elif next_value == '(':
      open_paren = tokens.pop()
      args = []
      while not tokens.pop_if_present(')'):
        if len(args) > 0:
          tokens.pop_expected(',')
        args.append(parse_expression(tokens))
      expr = FunctionInvocation(expr, open_paren, args)
    elif next_value == '[':
      open_bracket = tokens.pop()
      index_value = parse_expression(tokens)
      expr = BracketIndex(expr, open_bracket, index_value)
      tokens.pop_expected(']')
    else:
      check_suffixes = False
  return expr

def parse_entity(tokens):
  if tokens.is_next('('):
    tokens.pop_expected('(')
    expr = parse_expression(tokens)
    tokens.pop_expected(')')
    return expr

  next_token = tokens.peek()
  if next_token == None: raise tokens.eof()
  next_value = next_token.value
  next_type = next_token.token_type

  if next_type == 'KEYWORD':
    if next_value in ('true', 'false'):
      tokens.pop()
      return BooleanConstant(next_token, next_value == 'true')
    if next_value == 'null':
      tokens.pop()
      return NullConstant(next_token)
    raise Exception(next_token, "Unexpected usage of '" + next_value + "'.")

  if next_type == 'WORD':
    tokens.pop()
    if next_value == 'this':
      return ThisConstant(next_token)
    return Variable(next_token, next_value)
  
  if next_type == 'FLOAT':
    tokens.pop()
    float_value = None
    try:
      float_value = float(next_value)
    except:
      raise ParserException(next_token, "Invalid expression (presumed to be a float): '" + next_value + "'.")
    return FloatConstant(next_token, float_value)
  
  if next_type == 'NUMBER':
    tokens.pop()
    int_value = None
    if next_value[:2] == '0x':
      try:
        int_value = int(next_value[2:])
      except:
        raise ParserException(next_token, "Invalid expression (presumed to be a hexadecimal integer): '" + next_value + "'.")
    else:
      try:
        int_value = int(next_value)
      except:
        raise ParserException(next_token, "Invalid expression (presumed to be a decimal integer): '" + next_value + "'.")
    return IntegerConstant(next_token, int_value)
  
  if next_type == 'STRING':
    tokens.pop()
    str_value = string_literal_to_value(next_token, next_value[1:-1])
    return StringConstant(next_token, str_value)
  
  if next_value == '[':
    array_items = []
    tokens.pop()
    next_item_allowed = True
    while not tokens.pop_if_present(']'):
      if not next_item_allowed:
        tokens.pop_expected(']') # intentionally throw
      array_items.append(parse_expression(tokens))
      next_item_allowed = tokens.pop_if_present(',')
    return ArrayDefinition(next_token, array_items)

  if next_value == '{':
    tokens.pop()
    dictionary_keys = []
    dictionary_values = []
    next_item_allowed = True
    while not tokens.pop_if_present('}'):
      if not next_item_allowed:
        tokens.pop_expected('}') # intentionally throw
      dictionary_keys.append(parse_expression(tokens))
      tokens.pop_expected(':')
      dictionary_values.append(parse_expression(tokens))
      next_item_allowed = tokens.pop_if_present(',')
    return DictionaryDefinition(next_token, dictionary_keys, dictionary_values)

  raise Exception(tokens.pop(), "Unexpected token: '" + next_value + "'.")

# Note that the order here is a little weird for the first few. parse_unaries is after exponents and before multiplication
parse_exponents = OpChainParser(['**'], parse_entity_with_suffix_chains).parse
parse_multiplication = OpChainParser(['*', '/', '%'], parse_unaries).parse
parse_addition = OpChainParser(['+', '-'], parse_multiplication).parse
parse_bitwise_op = OpChainParser(['<<', '>>'], parse_addition).parse
parse_inequality = OpChainParser(['<', '>', '<=', '>='], parse_bitwise_op).parse
parse_equality = OpChainParser(['==', '!='], parse_inequality).parse
parse_bitwise_op = OpChainParser(['&', '|', '^'], parse_equality).parse
parse_boolean_combinator = OpChainParser(['&&', '||'], parse_bitwise_op).parse
parse_null_coalescer = OpChainParser(['??'], parse_boolean_combinator).parse

def parse_ternary(tokens):
  root = parse_null_coalescer(tokens)
  if not tokens.is_next('?'):
    return root
  
  question_mark = tokens.pop_expected('?')
  true_expr = parse_ternary(tokens)
  tokens.pop_expected(':')
  false_expr = parse_ternary(tokens)
  return TernaryExpression(root, question_mark, true_expr, false_expr)
