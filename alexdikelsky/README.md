# JMod
The goal is to be like the J programming language,
but with syntax level support for modular arithmetic,
and with S-expressions to make parsing easier for now.
It also requires the rank of the some operators be specified,
unlike J. For instance:

```
> 3m4
3m4

> 5m4
1m4

> (+ 1 3m4)
0m4

> (+ 1 [0 3m4 6m2])
[1 0m4 1m2]

> (/ + 0 '(1m2 0m2 1m2)))
0m2

> (i. 4)
[0 1 2 3]

> (+ 0m4 (i. 8))
[0m4 1m4 2m4 3m4 0m4 1m4 2m4 3m4 ]

```

Lambdas are supported

```
> ((λ (x) (+ x 3)) [4 5])
[7 8]

> (((λ (f g) (λ (x) (f (g x)))) (λ (x) (/ + 0 x)) |:) [[3 4] [5 6]])
[7 11]
```

# Installation and Execution
1. Install idris2, following the directions here: https://github.com/idris-lang/Idris2/blob/main/INSTALL.md
2. Clone this repository
3. Move to the `jmod` directory
4. Run `idris2 Main.idr --package contrib` to start the repl.
5. To run a program from a file, type
```
:exec main
<filename>
```

For example,
```
alex@alex-virtual-machine ~/programming-languages/jam0001/other-thing/jmod2/alexdikelsky/jmod
 % idris2 Main.idr --package contrib
     ____    __     _         ___                                           
    /  _/___/ /____(_)____   |__ \                                          
    / // __  / ___/ / ___/   __/ /     Version 0.4.0-4a9f00078
  _/ // /_/ / /  / (__  )   / __/      https://www.idris-lang.org           
 /___/\__,_/_/  /_/____/   /____/      Type :? for help                     

Welcome to Idris 2.  Enjoy yourself!
Main> :exec main
Enter the filename: fizzbuzz.jmod
233168
```
