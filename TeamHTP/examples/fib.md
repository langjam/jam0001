Run `fib calc: 10` in REPL then come back to see the result in the following code block

First 10 numbers of the Fibonacci sequence:

[fib]::
```
```

[fib]:: (
    calc: count
        | fib | fib := @{}.
        fib push: 0.
        fib push: 1.
        ${2 to: count} forEach: [ :i |
            fib push: ${fib at: i - 2} + ${fib at: i - 1}
        ].
        fib forEach: [ :num | 
            this append: num; append: '\n'.
        ].
)
