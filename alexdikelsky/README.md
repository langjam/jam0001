# JMod
The goal is to be like the J programming language,
but with syntax level support for modular arithmetic,
and with S-expressions to make parsing easier for now.
It also requires the rank of the some operators be specified,
unlike J. For instance:

```
> 3_4
3_4

> 5_4
1_4

> 1 + 3_4
0_4

> (+ 1 '(0 3_4 6_2))
(1 0_4 1_2)

> ((" / 1) + '(1_2 0_2 1_2))
0_2

> (i. 4 4)
(0_4 1_4 2_4 3_4)

> (i. 4 2)
(0_2 1_2 0_2 1_2)

> (i. 4_2 2)
Type error: First argument to i. should be [Number]

> (:= y ($ '(2 2) (i. 4 4)))
> y
((0_4 1_4) 
 (2_4 3_4))

> ((" / 0) + y)   NB. Sum across every element (doesn't do anything)
((0_4 1_4) 
 (2_4 3_4))

> ((" / 1) + y)   NB. Sum across the first list
(1_4 1_4)

> ((" / 2) + y)   NB. Sym across the first column
(2_4 0_4)
```


First-in-class comments are created with `NB.` until the end of line.
