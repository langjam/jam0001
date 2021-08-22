// 0 - closed, 1 - open
// $-1 door array
// $-2 index
// $-3 iteration

set($-1 grow("" 100))

set($-2 0) // return
set($-2 m("$-2 + 1")) // move
set($-1 place($-1 $-2 0)) // close
set($-1 place($-1 $-2 1)) // open

peek($-1 $-2) // swap cond

set($0 inz("swap cond"() $0 0 "close")) // swap
and(set($0 inz(not("swap cond"()) $0 0 "open"))) // swap
and($0()) // swap

"move and swap"() // iteration 0
"move, move and swap"() // iteration 1
"move, move, move and swap"() // iteration 2

"return"()

while (m("$-2 < 100")) {
  "iteration 0"()
}

"return"()

while (m("$-2 < 100")) {
  "iteration 1"()
}

"return"()

set($-3 0)
while (m("$-3 < 98")) {
  while (m("$-2 < 97")) {
    "iteration 2"()
  }

  "return"()
  set($-3 m("$-3 + 1"))
}

print($-1)
