
while (m("$-1 < 10")) {
  print($-1)
  set($-1 m("$-1 + 1"))
}
