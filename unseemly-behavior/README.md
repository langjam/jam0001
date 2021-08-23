Traditional comments explain code statically. First-class comments (we propose) explain code dynamically, at runtime.

Commentary is a language in which comments can also serve a logging function.
Commentary's syntax is stolen from Rust, thought it's quite limited.


If you run a program with the environment variable `COMMENTARY_SEARCH` set to "foo",
 it will print all comments containing "foo".

`sudo apt install cargo  # Or your distro's equivalent`
`cargo install unseemly  # Install Unseemly, the language Commentary is written in.`
`COMMENTARY_SEARCH='adding'   unseemly commentary.unseemly demo.commentary`

Inside comments, backticks indicate code that will be executed
 and interpolated into the printed comment.
Code inside backticks is typechecked, just like the surrounding code. Try:
`unseemly commentary.unseemly type_error.commentary`

To demonstrate two different implementations of the fibonacci function, try:

`COMMENTARY_SEARCH='evaluating' unseemly commentary.unseemly slow_fib.commentary`

`COMMENTARY_SEARCH='evaluating' unseemly commentary.unseemly fast_fib.commentary`

Commentary is written in [Unseemly](https://github.com/paulstansifer/unseemly),
 a language for typed language-oriented programs.
It is implemented as a set of Unseemly macros, though the surface syntax is entirely different.
(Thank goodness; Unseemly's syntax is bonkers.)

Note that, even though Commentary inherits Unseemly's type system,
 you get errors in terms of the Commentary code you wrote, not the Unseemly code it compiles to.

Due to a limitation in Unseemly, multiple interpolations in a comment are only supported
 if they have no text between them. ) :

We didn't have much time to implement Commentary, so it only has:
 * Integer and string literals (`1` and `"hi"`)
 * Function application (`plus(a, 1)`)
 * Lambdas (`|a: Int, b: Int| plus(a,b)`)
 * Function definitions (`fn addone(a: Int) -> Int { plus(a, 1) }`)
 * Let binding (`let x = 6;`)
 * Comments (`/* this is a comment! */`)
   (They must be positioned before an expression, so we know when to "evaluate" them.)
 * A `fix` form for creating recursive functions.

Due to a limitation in Unseemly, only 1- and 2- argument functions are supported.
Also, "normal" recursive functions sadly don't work,
 so you need to use the `fix` form to define them.


The implementation is in `commentary.unseemly`.

`build_a_language.unseemly` is a library mostly pre-existing code
 that adds some very basic features to Unseemly, which is *extremely* barebones.
In fact, there's a lot of overlap between the two files, when the same feature is being added to both languages.

