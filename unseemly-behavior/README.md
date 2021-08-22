Traditional comments provide insight into code at runtime. First-class comments provide insight dynamically. 

Commentary is a language in which comments can also serve a logging function.
Commentary's syntax is stolen from Rust, thought it's quite limited.

Commentary is written in [Unseemly](https://github.com/paulstansifer/unseemly),
 a language for typed language-oriented programs.
It is implemented as a set of Unseemly macros, though the surface syntax is entirely different.
(Thank goodness; Unseemly's syntax is bonkers.)

If you run a program with the environment variable `COMMENTARY_SEARCH` set to "foo",
 it will print all comments containing "foo".

`sudo apt install cargo`
`cargo install unseemly`
`COMMENTARY_SEARCH='adding' unseemly commentary.unseemly demo.commentary`

Inside comments, backticks indicate code that will be executed
 and interpolated into the printed comment.
Code inside backticks is typechecked, just like the surrounding code. Try:
`unseemly commentary.unseemly type_error.commentary`

Note that, even though Commentary inherits Unseemly's type system,
 you get errors in terms of the Commentary code you wrote, not the Unseemly code it compiles to.

Due to a limitation in Unseemly, multiple interpolations in a comment are only supported
 if they have no text between them. ) :

I didn't have much time to implement Commentary, so it only has:
 * Integer and string literals (`1` and `"hi"`)
 * Function application (`plus(a, 1)`)
 * Lambdas (`|a: Int, b: Int| plus(a,b)`)
 * Function definitions (`fn addone(a: Int) -> Int { plus(a, 1) }`)
 * Let binding (`let x = 6;`)
 * Comments (`/* this is a comment! */`)
   (They must be positioned before an expression, so we know when to "evaluate" them.)

Due to a limitation in Unseemly, only 1- and 2- argument functions are supported.
Also, to my great embarassement, I discoverd that recursive functions don't quite work,
 and I didn't have time to implement a recurisve `let` to implement them with.

The implementation is in `commentary.unseemly`. `build_a_language.unseemly` is mostly 
pre-existing code that adds some very basic features to Unseemly, which is *extremely* barebones. 

