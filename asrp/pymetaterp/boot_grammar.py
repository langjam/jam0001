bootstrap = r"""
name = (letter | '_') (letter | digit | '_')*
expr = apply | exactly | token | parenthesis | output

exactly! = "'" {(escaped_char | ~'\'' anything)*} "'"
token! = "\"" {(escaped_char | ~'"' anything)*} "\""
escaped_char! = '\\' {'n'|'r'|'t'|'b'|'f'|'"'|'\''|'\\'}
apply! = ('\t'|' ')* {name}
parenthesis = "(" {or} ")"
output! = "{" {or} "}"

not = "~" {expr=negation} | expr
quantified = not (('*' | '+' | '?')=quantifier)?
bound = quantified ('=' {name=inline})?
and = bound*
or = and ("|" {and})*

rule = spaces {name=rule_name '!'?=flags and=args ("=" {or})}
grammar = {rule*} spaces
"""

diff = r"""
comment = '#' (~'\n' anything)*
hspace = ' ' | '\t' | comment
indentation = (hspace* ('\r' '\n' | '\r' | '\n'))* hspace+
space = '\n' | '\r' | hspace

expr = apply | exactly | token | parenthesis | output | list
     | rule_value | predicate | action

list! = "[" {or} "]"
predicate! = "?(" {balanced} ')'
action! = "!(" {balanced} ')'
rule_value! = "->" hspace* {(~'\n' ~'<' anything)*} '<'?
apply! = indentation? {name ('(' {balanced=args} ')')?}
not = "~" "~" {expr=lookahead} | "~" {expr=negation} | expr
bound = ":" {name=bind}
      | quantified (':' {name=bind} | '=' {name=inline})?

balanced = (escaped_char | '(' balanced ')' | ~')' anything)*
"""

extra = """
letter = 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'
digit = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
space = '\t'|'\n'|'\r'|' '
spaces = space*
"""
