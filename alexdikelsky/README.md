# JMod
The goal is to be like the J programming language,
but with syntax level support for modular arithmetic.
For instance:

```
> 3_4
3_4

> 5_4
1_4

> 1 + 3_4
0_4

> 1 + [0 3_4 6_2]
[1 0_4 1_2]
```

First-in-class comments are created with `;` until the end of line.
