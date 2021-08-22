from exceptions import UnboundVariable;
from jlast import AstVisitor
from prelude import prelude


class Resolver(AstVisitor):
    def __init__(self, env=None):
        super().__init__()
        self.scopes = []
        if env is None:
            self.scopes.append(set(prelude.bindings.keys()))
            self.scopes.append(set())
        else:
            while env is not None:
                self.scopes.insert(0, set(env.bindings.keys()))
                env = env.parent

    def begin_scope(self):
        self.scopes.append(set())

    def end_scope(self):
        self.scopes.pop(-1)

    def visit_program(self, b):
        for stmt in b.exprs:
            self.visit(stmt)
        
    def visit_block(self, b):
        self.begin_scope()
        for stmt in b.exprs:
            self.visit(stmt)
        self.end_scope()

    def visit_commented_expr(self, e):
        self.visit(e.expr)
        self.visit(e.comment)

    def visit_assignment(self, a):
        self.visit(a.name)
        self.visit(a.expr)
        
    def visit_declaration(self, a):
        self.scopes[-1].add(a.name.name)
        self.visit(a.name)
        self.visit(a.expr)

    def visit_literal(self, l):
        pass

    def visit_name(self, n):
        for i in range(len(self.scopes)):
            if n.name in self.scopes[-i - 1]:
                n.binding_depth = i
                return
        raise UnboundVariable(n)

    def visit_bin_expr(self, e):
        self.visit(e.lhs)
        self.visit(e.rhs)
        
    def visit_unary_expr(self, e):
        self.visit(e.expr)
 
    def visit_and_expr(self, e):
        self.visit(e.lhs)
        self.visit(e.rhs)
        
    def visit_or_expr(self, e):
        self.visit(e.lhs)
        self.visit(e.rhs)

    def visit_call(self, c):
        self.visit(c.f)
        for a in c.args:
            self.visit(a)
        
    def visit_fn_expr(self, f):
        self.begin_scope()
        for p in f.params:
            self.scopes[-1].add(p.name)
        self.visit(f.body)
        self.end_scope()
                
    def visit_explain_expr(self, c):
        self.visit(c.expr)

    def visit_while_expr(self, e):
        self.visit(e.cond)
        self.visit(e.body)

    def visit_if_expr(self, e):
        self.visit(e.cond)
        self.visit(e.then_body)
        if e.else_body is not None:
            self.visit(e.else_body)
