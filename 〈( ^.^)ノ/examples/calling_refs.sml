
print("hello world") // hello

m("$1 * $1") // square

set($-1 "hello")

// you can call a reference, if it contains a comment
$-1()

// inz is inset if not zero. it can be used for flow control
set($-1 inz(TRUE $-1 1 "square"))

// this calls both "hello" and "square"
print($-1(10))
