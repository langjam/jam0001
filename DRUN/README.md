# DRUN
DRUN is a toy programming language designed during first langjam of 2021. The theme of jam is `first class comments`.

DRUN implements this idea in a way that comments are:
- describing and affecting runtime
- describing and affecting behaviour of code
- code can reach into comments and change its behaviour based on that

For example here is code that can access it's own comment and comment of functions that called it to check if author is `kfirmanty` and based on that execute code differently

```
#author kfirmanty
FN ADD(a,b)
 IF ALL_COMMENTS(\"AUTHOR\") CONTAINS \"kfirmanty\" RETURN NIL END
 RETURN a + b
END
```

Here something similar is implemented purely in the comments. If `SUB` is called by any code which is authored by `kfirmanty` it will immediately return value of 5. The comment can contain any valid `DRUN` code
```
#if called from code by kfirmanty: RETURN 5 END
#execute 5 to 10 times
FN SUB(a,b)
  RETURN a - b
END
```

Here another change of behaviour is introduced - if branches can be reversed in order based on comment which can results in interesting behaviour ;)
```
#if calls have 40% possibility of being reversed in this function and descendants
FN ADD_IF_SMALL(a,b)
 IF a > 2 
  RETURN 2 + b
 ELSE
  RETURN a + b
 END
END
```

Even more for example all variables can be defined as immutable by comment:
```
#all variables are immutable
FN ADD_2(a)
  a = a + 2
  RETURN a
END
```
^ in that case the result will still be original value passed to `a`. All operations that would breake immutability would be ignored.

It should also be possible to set comments using `env` variables. For example setting `DRUN_MATH_ADD_2=all variables are mutable` would override previous comment and variables would be mutable again. Also using `${}` syntax comments could access `env` variables.

```
#if author is ${SOME_AUTHOR}: RETURN NIL END
```
