(ns langjam.parser
  (:require [instaparse.core :as insta]))

(def parser (insta/parser "
<S> = TERM+
<TERM> = (FN-DEF | COMMENT) '\n'?

COMMENT = <'#'> (<WS> | #'\\w+')+ <'\n'>

FN-DEF = <'FN'> <WS> FN-NAME ARG-LIST <WS> <'\n'?> (EXPRESSION <'\n'?>)+ <'END'>
FN-NAME = #'[A-Z]+' #'[A-Z0-9_]*'
ARG-LIST = <'('> (VAR <WS>?)* <')'>

<EXPRESSION> = ASSIGNMENT | FN-CALL | RETURN | IF-COND

ASSIGNMENT = VAR <WS>? <'='> <WS>? (FN-CALL | VAL)

RETURN = <'RETURN'> <WS> (VAR | FN-CALL | VAL)

FN-CALL = FN-NAME <'('> ((VAR | FN-CALL | VAL) <WS>?)* <')'>

IF-COND = <'IF'> <WS> IF-TEST <WS>? <'\n'?> IF-BLOCK <WS>? ELSE-BLOCK <'ENDIF'>
IF-TEST = ((VAR | FN-CALL | VAL) <WS>?) TEST-OP <WS>? ((VAR | FN-CALL | VAL) <WS>?)
TEST-OP = '==' | '<' | '<=' | '>=' | '>'
IF-BLOCK = (EXPRESSION <'\n'?>)+
ELSE-BLOCK = (<'ELSE'> <WS>? <'\n'?> (EXPRESSION <'\n'?>)+)?

VAR = #'[a-z]+' #'[a-z0-9_]*'
VAL = NUMBER | STRING
NUMBER = #'[0-9]+'
STRING = <'\"'> (WS | #'\\w+' | #'\\d+')+ <'\"'>

ANL=<'\n'?>
WS = #'\\s+'"))
