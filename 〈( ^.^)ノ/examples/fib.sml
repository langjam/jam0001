
set($-1 1)
set($-2 1)

set($-3 $-2) // save
set($-2 m("$-2 + $-1")) // calc
set($-1 $-3) // load

"save, calc and load"() // fib

while (m("$-1 != 2584")) {
  "fib"()
  print($-1)
}
