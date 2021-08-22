#!/usr/bin/env python3

import lark
import sys
from dataclasses import dataclass

grammar = r"""
program : (expr ";")* [expr]


COMMENT : /\/\*(\*(?!\/)|[^*])*\*\//
TRUE: "True"
FALSE: "False"
%import common.WS
%import common.SIGNED_NUMBER
%import common.ESCAPED_STRING
%import common.CNAME
%ignore WS

?expr : commented_expr
?commented_expr : or_expr commented_expr
                | or_expr
!?or_expr : or_expr "|" and_expr -> bin_expr
         | and_expr
!?and_expr : and_expr "&" cmp_expr -> bin_expr
          | cmp_expr
!?cmp_expr : cmp_expr ("==" | "<" | ">")  mul_expr -> bin_expr
         | add_expr
!?add_expr : add_expr ("+" | "-") mul_expr -> bin_expr
          | mul_expr
!?mul_expr : mul_expr ("*" | "/" | "%") unary_expr -> bin_expr
           | unary_expr
!?unary_expr : ("!" | "-") unary_expr
             | prim_expr
?prim_expr : COMMENT
           | TRUE
           | FALSE
           | SIGNED_NUMBER
           | ESCAPED_STRING
           | group_expr
           | unit
           | name
           | assignment
           | declaration
           | if_expr
           | while_expr
           | call_expr
           | fn_expr
           | explain_expr
           | block

?group_expr  : "(" expr ")"
!unit        : "(" ")"
assignment   : name "=" expr
declaration  : "let" name "=" expr
explain_expr : prim_expr "?"
name : CNAME

if_expr    : "if" group_expr expr ["else" expr]
while_expr : "while" group_expr expr
call_expr  : prim_expr "(" [expr ("," expr)*] ")"
fn_expr    : "fn" "(" [name ("," name)*] ")" expr
block      : "{" (expr ";")* [expr]"}"

"""

parser = lark.Lark(grammar, start='program', parser='lalr', propagate_positions=True)
