// ------------------
// smile-lang example
// ------------------
set($-1 1)
set($-2 1)
set($-1 $3) // fib
and(set($-2 m("$2 + $3"))) // fib
while (TRUE) {
  "fib"($-1 $-2)
  print($-2)
} 
