The @fibonacci function is a famous function which implements this algorithm:
$F_n = F_{n - 1} + F_{n - 2}$

In the future, it might be desirable to update this fibonacci implementation
to be a more optimized form. The two main ways of doing so would be to:

\begin{itemize}
	\item Modify this recursive fibonacci function to memoize its output; or
	\item Rewrite the function to use an iterative algorithm rather than a recursive one.
\end{itemize}

\fun{fibonacci}{n}{
	if n <= 2 {
		return 1;
	} else {
		return fibonacci(n - 1) + fibonacci(n - 2);
	}
}

The @main function is the entry point of our program.
This @main function loops through all the fibonacci numbers
from 1 to 10 and prints them.

\fun{main}{}{
	n := 1;
	while n <= 10 {
		print("fib(" + n + "):", fibonacci(n));
		n = n + 1;
	}
}
