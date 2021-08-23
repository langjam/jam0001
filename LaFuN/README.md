# LaFuN programming language

LaFuN empowers you with the most **expressive** comments you have ever seen.
We have combined the power of LaTeX with a simple Javascript-like sublanguage named FuN
to bring you a new comment-writing experience that will improve code readability for everyone.

Here's a simple demonstration of what LaFuN brings to the table.

```
\section{Introduction}

The $L^2$-norm, or Euclidean distance, is often used to measure the distance
between two points in an $n$-dimensional vector space.
However, it can be slow to compute as it requires finding the square root of a number.
As such, it is common to use the squared $L^2$-norm instead,
which is often notated $\left\Vert \cdot \right\Vert_2^2$.

\section{Implementation}

Here is an implementation of the squared $L^2$-norm for 2D vectors.

\fun{SqL2Norm2D}{v}{
  \fun{Sq}{n}{ return n * n; }
  SqX := Sq(v.x);
  SqY := Sq(v.y);
  return SqX + SqY;
}

\section{Usage}

To use @SqL2Norm2D, you will need a class that encapsulates a vector.

\class{Vector2}{x, y}{
  self.x = x;
  self.y = y;
}

Let's test it out!

\fun{main}{}{
  vector := Vector2(3, 4);
  print("vector: x=" + vector.x + ", y=" + vector.y);
  print("SqL2Norm2D(vector):", SqL2Norm2D(vector));
}

This program should print $25$ to the console when run.

\section{Conclusion}

@SqL2Norm2D is a really simple function and is fast on most architectures.
Note that the factoring out of the @Sq function
may lead to a slight slowdown due to function call overhead.
As such, it may be good to consider
inlining the entire computation into a single return statement.
```

When compiling to LaTeX then to PDF, we can see just how beautiful our code has become:

![Sample LaFuN output](/images/readme-sample-latex-output.png)

Running it is also simple!

```
$ ./build/lafun examples/readme.fun -o test.js
$ node test.js
vector: x=3, y=4
SqL2Norm2D(vector): 25
```

# Try LaFuN

To try LaFuN, simply clone this repository and try some of the example commands below to get started!

```
$ make
$ ./build/lafun examples/fibonacci.fun -o test.js
$ node test.js
vector: x=3, y=4
SqL2Norm2D(vector): 25
$ ./build/lafun examples/readme.fun --latex test.tex
$ # Compile test.tex with your LaTeX compiler of choice (e.g. Overleaf)
```

If you wish to use a custom LaTeX prelude, use `--no-latex-prelude`.

```
$ ./build/lafun examples/readme-with-prelude.fun --no-latex-prelude --latex test.tex
```

It *should* work with any modern C++ compiler which supports C++17 or newer.
The Makefile also assumes a compiler with a GCC-like interface.
It has been tested on Ubuntu 21.04 with GCC 12.2.0 and with Clang 12.0.1,
and on macOS with Apple Clang 12.0.5.

# The Language

LaFuN actually consists of two "languages"; the outer language, called LaFuN,
and the inner language, called FuN. The LaFuN parser handles all the LaTeX
parsing and detects `\fun` and `\class` definitions, and hands them over to the
FuN parser.

The FuN parser is a hand-written recursive descent parser, with a hand-written
lexical analyzer. The back-end is a code generator which generates JavaScript.

FuN has functions:

```
\fun{functionName}{argument1, argument2}{
	<body>
}
```

The body of a function is a list of statements. The statements are:

* A function or class declaration: `\fun{name}{arguments}{body}` or `\class{name}{args}{body}`
* Any expression, followed by a semicolon
* An if statement: `if <expression> { <body> }`
* An if statement with an else-if part: `if <expression> { <body> } else if <expression2> { <else-body> }`
* An if statement with an else part: `if <expression> { <body> } else { <else-body> }`
* A while statement: `while <expression> { <body> }`
* A return statement: `return <expression>`

The supported expression types are:

* Identifiers: `foo`
* Number literals: `100`, `0xff`
* String literals: `"Hello World"`
* Binary expressions: `<expr> <operator> <expr>`; like `"Hello " + name`
* Function calls: `sayHelloTo("Mary")`
* Declaration assignments: `name := "Bob"`
* Assignments to an existing variable: `name = "Alice"`
* Look-ups by name: `someObject.someProperty`
* Assignment through property names: `person.name = "Carol"`

In addition to functions, FuN has classes:

```
\class{ClassName}{argument1, argument2}{
	<body>
}
```

The body of a class definition is its constructor function, and its arguments
are the arguments to the constructor. The constructor also has an implicit variable
called `self`, which represents the object being constructed.

Here's an example class:

```
\class{Vector2D}{x, y}{
	self.x = x;
	self.y = y;
}
```

When we have a class, we can add methods to it with a `\fun` declaration:

```
\fun{Vector2D::scale}{k}{
	self.x = self.x * k;
	self.y = self.y * k;
}

\fun{Vector2D::add}{other}{
	self.x = self.x + other.x;
	self.y = self.y + other.y;
}

\fun{Vector2D::toString}{}{
	return "Vector2D{" + self.x + ", " +self.y + "}";
}
```

Creating an instance of a class is just like aclling a function,
so `Vector2D(10, 20)` creates a vector with `x=10` and `y=20`.

Here's an example program using our vector:

```
\fun{main}{}{
	vec := Vector2D(10, 20);
	vec.scale(0.5);
	vec.add(Vector2D(5, 5));
	print(vec.toString());
}
```

This project is also hosted at https://github.com/mortie/lafun-language
