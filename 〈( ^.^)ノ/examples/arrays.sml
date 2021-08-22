
// grow expands array by the argument
print("grow:")
set($1 grow($1 10))
print($1)

// place set element with a value
print("place:")
set($1 place($1 2 "hello"))
print($1)

// peek gets value at an index
print("peek:")
print(peek($1 2))
