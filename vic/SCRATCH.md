
Theme: The theme of the jam is "first-class comments". for whatever "first-class" means.
https://discord.com/channels/862925437013721088/862925437743005718/878346255850434560


first-class:
 - can be used as a value
 - can be constructed / modified.
 
things I dont want:
 - comments be glorified strings.



some ideas:
  - explore how to improve doctests or the like.
  - make a troll language where comments are actual expressions
    and non-commented expressions are "data" or comments if not evaluated at all.
  - define contracts about functions, like in a concatenative language the pre and post stack-state


Foo - is a concatenative programming language

Inspirations:
  Doctest - Elixir

  #  This function does bla,
  #  Example:
  # 
  #  ```
  #  add(1, 2) => 3
  #  ```


  # Esto suma 5
  add(1, 2) => 3


  1 2 
  # add



  # 
  add(a, b) = a + b



  #
  # for all ints a and all ints b, add(a, b)
  add(a, b) = a + b
   
 
  Racket - lisp - (add 1 2) 

  Factor and all other concat langs - https://concatenative.org
        point-free pointless 

        foo.txt
	```
	# Hola
	1  2  add(_, _)

        # duplica dos veces el top del stack

	dup2dsadsa   =    dup   dup 2221 9421219,.321312312 "hoila"  "hola  mundo"

	sd2a = swap dup2 add

        # eososos 
	aa = 4 5 swap dup2 sd2a

	1 2 3 


	```

[ 1 2 9 3 6 ]

# shuffling operators
// swap: [ a b ... ] -> [ b a ... ]


- lexer/parser  Text => AST


What is a comment ?


```

 a a /* add(a, b) = a + b */ b b 
add 1 2

"""
Hola esto es una doc
"""


(define xx (a b) 
  ;; This is ignored 
  "This is xx"
  (sum a b))

```

