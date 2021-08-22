# A second test document

This file isn't really a snazzy demo of the markdown integration. It's more of just a means to have one-off coverage on some features as I develop them.

```

testWhileLoop();
testDoWhileLoop();

sumTest = sumNumbers1To100();
print("Sum of the numbers from 1 to 100:", sumTest);

print(ternaryTest('apple', 4));
print(ternaryTest('apple', 1));
print(ternaryTest('apple', 0));

print("5! = " + factorialRecursive(5));

testIncrement();
dictionaryTest();
```

### Dictionary Test

```
dict = {
    "a": 1,
    "b": 2,
};

assert(dict["a"] == 1, "checking key 'a'");
assert(dict["b"] == 2, "checking key 'b'");

dict['c'] = 3;
assert(dict['c'] == 3, "checking key 'c'");

assert(dict.keys() + '' == '[a, b, c]', "checking keys");
assert(dict.values() + '' == '[1, 2, 3]', "checking values");

dict['b'] = 4;
assert(dict.keys() + '' == '[a, b, c]', "checking keys");
assert(dict.values() + '' == '[1, 4, 3]', "checking values");

print("Dictionary tests passed");

```

### Test Increment

```
a = 10;
b = a++;
assert(a == 11, "a++");
assert(b == 10, "b = a++");
c = a--;
assert(a == 10, "a--");
assert(c == 11, "c = a--");
a += 1;
assert(a == 11, "a += 1");

a = 10;
b = ++a;
assert(a == 11, "++a");
assert(b == 11, "b = ++a");
c = --a;
assert(a == 10, "--a");
assert(c == 10, "c = --a");

a = [10];
b = a[0]++;
assert(a[0] == 11, "a[0]++");
assert(b == 10, "b = a[0]++");
c = a[0]--;
assert(a[0] == 10, "a[0]--");
assert(c == 11, "c = a[0]--");

a = [10];
b = ++a[0];
assert(a[0] == 11, "++a[0]");
assert(b == 11, "b = ++a[0]");
c = --a[0];
assert(a[0] == 10, "--a[0]");
assert(c == 10, "c = --a[0]");
a[0] += 1;
assert(a[0] == 11, "a[0] += 1");

a = DummyObject.init();
a.num = 10;
b = ++a.num;
assert(a.num == 11, "++a.num");
assert(b == 11, "b = ++a.num");
c = --a.num;
assert(a.num == 10, "--a.num");
assert(c == 10, "c = --a.num");

a.num += 1;
assert(a.num == 11, "a.num += 1");

a = { 'num': 10 };
b = ++a['num'];
assert(a['num'] == 11, "++a['num']");
assert(b == 11, "b = ++a['num']");
c = --a['num'];
assert(a['num'] == 10, "--a['num']");
assert(c == 10, "c = --a['num']");

a['num'] += 1;
assert(a['num'] == 11, "a['num'] += 1");

print("Increment tests passed");

```

### factorial recursive

- `n` - the value to calculate the factorial of

```
if n < 2 {
    return 1;
}
return factorialRecursive(n - 1) * n;
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

## Dummy Object

This object has no fields or methods.
