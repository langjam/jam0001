<!--
(add-fast a b) is
(if (= a 0)
  b
  (add-fast (- a 1) (+ b 1)))
-->
This will take a while, but won't overflow the stack:
``(add-slow 1000000 2)``

<!--
(add-slow a b) is
(if (= a 0)
  b
  (+ 1 (add-slow (- a 1) b)))
-->
If you increase this to match the first function, then you will overflow the stack, probably:
``(add-slow 10 2)``
