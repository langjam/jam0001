# A second test document

This file isn't really a snazzy demo of the markdown integration. It's more of just a means to have good coverage on all features.

```

testWhileLoop();
testDoWhileLoop();

sumTest = sumNumbers1To100();
print("Sum of the numbers from 1 to 100:", sumTest);

print(ternaryTest('apple', 4));
print(ternaryTest('apple', 1));
print(ternaryTest('apple', 0));
```

### ternary test

- `word` - word to make plural conditionally
- `num` - number of items
```
return num == 1 ? '1 ' + word : num + ' ' + word + 's';
```

### test while loop

```
i = 0;
while i < 10 {
    print("while: " + i);
    i += 2;
}
```

### test do-while loop

```
i = 4;
do {
    print(i);
    i--;
} while(i >= 0);
```

### sum numbers 1 to 100

```
total = 0;
for i = 1 thru 100 {
    total += i;
}
return total;
```