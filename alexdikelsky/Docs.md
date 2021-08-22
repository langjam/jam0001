Name | Definition | Number of args
---------------|--------------------|---------------
`+` | Add two things together. For instance `(+ 2 3 4)` results in `9`. You can add vectors together `(+ [1 2] [3 4])` results in `[4 6]` | Any
`*` | Same as addition, but multiplication instead | Any
`%` | This divides numbers. When one argument is `Finite n modulus`, it takes the modular inverse. | Any
`/` | This is the fold operation. It is used to aggregate a value from an array. For instance, `(/ + 0 [1 2 3])` means the same as `0 + 1 + 2 + 3`, which is `6`. | `(/ function identity list)`
`λ` | This is a lambda expression. It uses static scope, and [follows Scheme's implementation](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-4.html#node_sec_1.6) | `(λ (a b ...) body)`
`let` | This works the same way [Scheme let statements work](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-4.html#node_sec_1.3): They provide syntactic sugar for reusing values | `(let ((name1 expr1) (name2 expr2) ...) bodyexpr)`
`i.` | This creates a range of nummbers from `0` to the first argument. `(i. 4)` is `[0 1 2 3]` | `(i. NaturalNumber)`
`=` | This checks equality. Truth is represented by `1 mod 2`, and false is `0 mod 2`. | Any
