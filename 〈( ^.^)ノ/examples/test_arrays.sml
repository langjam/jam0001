
print("grow:")
set($1 grow($1 10))
print($1)

print("place:")
set($1 place($1 2 "hello"))
print($1)

print("peek:")
print(peek($1 2))
