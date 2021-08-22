Run `fib calc` in REPL then come back to see the result in the following code block:

[fib]::
```
```

[fib]:: (
    calc
        | fib | fib := @{}.
        fib push: 0.
        fib push: 1.
        ${2 to: 20} forEach: [ :i |
            fib push: ${fib at: i - 2} + ${fib at: i - 1}
        ].
        fib forEach: [ :num | 
            this append: num; append: '\n'.
        ].
)
