# Loop

```rundown
let global i = 0;
i = i + 1;

if (i == 100) {
    sleep(5);
    goto "end";
}

if (((i % 3) == 0) and ((i % 5) == 0)) {
    goto "fizzbuzz";
}

if ((i % 3) == 0) {
    goto "fizz";
}

if ((i % 5) == 0) {
    goto "buzz";
}

print(i);
goto "loop";
```

# Fizz

Fizz

```rundown
goto "loop";
```

# Buzz

Buzz

```rundown
goto "loop";
```

# FizzBuzz

FizzBuzz

```rundown
goto "loop";
```

# End

Thanks!

```rundown
sleep(100);
```
