# FCC - Lisp with first-class comments

`Usage: cargo run -- [file]`

FCC (first-class comments) is a language from Lisp family, based mostly on
Scheme.

Implements comments as values, similar to strings. Comments are delimited by `;`
from both sides. So `; this comment ;` works. Top-level comments are simply
ignored, while comments in a list are considered a value.

Two new operand, `get-doc` and `set-doc` allow for access to variable's
comments.

```
; "define" accepts comment as optional first argument ;
(define ; a counter ; x 0)
;       ^^^^^^^^^^^^^ comment ;

(println "Docs for x: " (get-doc 'x))

(define succ (lambda (a) (+ a 1)))
; Oops, forgot to add comment ;
(set-doc 'succ ; Returns next natural number ;)

; assert: 4 ;
(+ 2 2)
```
