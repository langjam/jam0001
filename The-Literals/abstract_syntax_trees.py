class Expr():
    pass

class Number(Expr):
    def __init__(self, value):
        self.value = value
    
    def __repr__(self):
        return f'NUMBER {self.value}'

class Variable(Expr):
    def __init__(self, varname):
        self.varname = varname
    
    def __repr__(self):
        return f'VAR {self.varname}'

class Binop(Expr):
    def __init__(self, operator, left_operand, right_operand):
        self.operator = operator    
        self.left_operand = left_operand
        self.right_operand = right_operand
    
    def __repr__(self):
        return f'({self.left_operand}) {self.operator} ({self.right_operand})'

class Comparison(Expr):
    def __init__(self, operator, left_operand, right_operand):
        self.operator = operator    
        self.left_operand = left_operand
        self.right_operand = right_operand