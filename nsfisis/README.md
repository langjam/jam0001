# Onigiri

# Requirements

* Ruby
  * I used `ruby 3.0.2p107 (2021-07-07 revision 0db68f0233) [x86_64-darwin19]`, but older version may work.
  * It is implemented in pure Ruby, so no external libraries are required.


# How to Run

```
$ ruby onigiri.rb examples/hello_world.onigiri
```

`examples` contains several example codes.


# Details

The Onigiri Language is a dynamically-typed script language, having first-citizen comments. It has `commentof` operator that can retrieve a comment of variable. A comment can be modified in a similar manner to a way to change other values. Here is a small example:

```
# comment
var i = 0;
var ci = commentof i;
print(ci);
ci = ci + " of i";
print(ci);
print(commentof i);
```

The interpreter outputs all variables' comment in the end of execution like this:

```
=========================
i
-------------------------
comment of i
```

A comment to an assignment statement is appended to the existing comment of the left hand side variable.

```
# 1
var y;
# 2
y = y + 1;
# 3
y = y + 2;

print(commentof y);
```

This code outputs "1\n2\n3".

See `GRAMMAR` for detailed grammar.


# License

MIT License
Copyright 2021 nsfisis
