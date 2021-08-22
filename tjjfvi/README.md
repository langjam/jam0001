# commentrace

> a language featuring a comment trace, written during a comment race

Hacked together in the last 4 hours of the jam.

## Usage

1. Install node/npm.
2. Run `npm i`
3. Run `npm run start <file> [input]`

## About

Write beautifully-commented pseudo-code, and get a trace of the execution of the code. The trace is outputted to stderr, and isn't part of the output of the program.

Unlike some of the other languages, comments in this lang are *real* comments; they don't affect the execution of the code.

Example fizz buzz program:
```
# We'll interpret the input as a number, and set that as the max.
$max = toNumber($input)
# In this case, the max is $max.

# We'll initialize i, our index, to 0.
$i = 0

# We'll also keep a string accumulator for our output.
$str = ""

# Then, we'll enter a loop:
repeat:
  # Now, we'll increase i.
  $i = $i + 1
  # In this iteration, i is $i. Is that too much? (the max is $max)
  if $i > $input:
    # It is too much! We'll output the string that's been accumulated.
    return $str
  else:
    # Good news: it's not!
    # Now we'll check if i is a multiple of 3.
    if $i % 3 == 0:
      # It is! Let's now check if it's a multiple of 5.
      if $i % 5 == 0:
        # Look at that -- it's also a multiple of 5!
        # We'll add "FizzBuzz" (and a trailing newline!) to our output accumulator.
        $str = $str + "FizzBuzz\n"
      else:
        # It's not. That's ok, we'll just add "Fizz" (and the ever-important newline) to the output.
        $str = $str + "Fizz\n"
    else:
      # It's not. But is it a multiple of 5?
      if $i % 5 == 0:
        # It is! That means we'll add "Buzz" (and a newline!) to the output accumulator.
        $str = $str + "Buzz\n"
      else:
        # It isn't. That means we have to add just "$i" (and a newline, of course) to the string. Bo-ring.
        $str = $str + $i + "\n"
```

Output:
```
We'll interpret the input as a number, and set that as the max.
In this case, the max is 100.
We'll initialize i, our index, to 0.
We'll also keep a string accumulator for our output.
Then, we'll enter a loop:
  Now, we'll increase i.
  In this iteration, i is 1. Is that too much? (the max is 100)
    Good news: it's not!
    Now we'll check if i is a multiple of 3.
      It's not. But is it a multiple of 5?
        It isn't. That means we have to add just "1" (and a newline, of course) to the string. Bo-ring.
  Now, we'll increase i.
  In this iteration, i is 2. Is that too much? (the max is 100)
    Good news: it's not!
    Now we'll check if i is a multiple of 3.
      It's not. But is it a multiple of 5?
        It isn't. That means we have to add just "2" (and a newline, of course) to the string. Bo-ring.
  Now, we'll increase i.
  In this iteration, i is 3. Is that too much? (the max is 100)
    Good news: it's not!
    Now we'll check if i is a multiple of 3.
      It is! Let's now check if it's a multiple of 5.
        It's not. That's ok, we'll just add "Fizz" (and the ever-important newline) to the output.
  Now, we'll increase i.
  In this iteration, i is 4. Is that too much? (the max is 100)
    Good news: it's not!
    Now we'll check if i is a multiple of 3.
      It's not. But is it a multiple of 5?
        It isn't. That means we have to add just "4" (and a newline, of course) to the string. Bo-ring.
...
1
2
Fizz
4
...
```
