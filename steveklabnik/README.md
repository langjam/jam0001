THEME: First class comments

My idea: homoiconic TOML

```toml
[[function]]
name = "main"
args = ["argc", "argv"]
vars = { x = "int", y = "string" }
body = [
    # print 'hello world'
    { print = "Hello world" },
    { print = ["The value of x is:", "$x"] },
]
```
