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

jsonTest();
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

dict['d'] = 5;
assert(dict.length == 4, "check length");
assert(dict.keys() + '' == '[a, b, c, d]', "checking keys");
assert(dict.values() + '' == '[1, 4, 3, 5]', "checking values");

dict.remove('a');
assert(dict.length == 3, "check length");
assert(dict.keys() + '' == '[d, b, c]', "checking keys");
assert(dict.values() + '' == '[5, 4, 3]', "checking values");

dict.remove('c');
assert(dict.length == 2, "check length");
assert(dict.keys() + '' == '[d, b]', "checking keys");
assert(dict.values() + '' == '[5, 4]', "checking values");

assert(dict.get('d') == 5, "getting a key that exists with .get()");
assert(dict.get('e') == null, "getting a key that doesn't exist with no fallback");
assert(dict.get('f', 'bananas') == 'bananas', "getting a key that doesn't exist with a fallback");

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

### Factorial Recursive

- `n` - the value to calculate the factorial of

```
if n < 2 {
    return 1;
}
return factorialRecursive(n - 1) * n;
```

### Ternary Test

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

### JSON Test

```
value = {
    "title": "I am flat JSON",
    "arr": [1, 2, 3],
    "objects": [
        {
            "id": 1,
            "value": "apples"
        },
        {
            "id": 2,
            "value": "bananas"
        },
    ],
    "floats": [1.5, 2.75],
};
print(json_serialize(value));
value['title'] = "I am *pretty* JSON!";
pretty_json = json_serialize(value, true);
print(pretty_json);
```

Do a quick check just to see if round-tripping a dictionary through the serializer and parser will result in the same value as the original.

```
roundTrippedValue = json_parse(pretty_json) + '';
directValue = value + '';
assert(roundTrippedValue == directValue, "JSON round-tripping");
```

## Dummy Object

This object has no fields or methods.
