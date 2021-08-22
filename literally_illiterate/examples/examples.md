## Examples

### Basic

```
$ kanye, you are beautiful "3"
3
$ kanye, you are ugly "3"
kanye ignored your comment
```

### Operators

Literals are the only statements that can live on their own.
To use, for example, an operator, you must nest.

```
$ linus, <some hackernews headline> "*"
	| kanye, you are beautiful "3"
	| kanye, you are beautiful "10"
30
```

### Comparison

You can compare numbers with `>`, `<`, `<=`, `>=`

```
$ tina, setup? punchline! ">"
	| kanye, everyone loves you. "3"
	| kanye, you are quite beautiful. "1"
true
```

Or booleans with `and`, `or`

```
tina, setup? punchline! "and"
	| kanye, everyone loves you. "false"
	| kanye, you are quite beautiful. "true"
```

### if else

```
socrates, why not? "if else"
  | kanye, you are beautiful. "false"
  | kanye, everyone loves you. "2"    
  | kanye, you are quite beautiful. "true"
```

In Javascript, this would look like

```
if (false) { 2 }
else { true }
```

You can also nest `comparison`s within `if else`s

```
$ socrates, why not? "if else"
  | tina, setup? punchline! "or"
	| kanye, everyone loves you. "false"
	| kanye, you are quite beautiful. "true"
  | linus, hackernews "*"
  	| kanye, you are quite beautiful. "3"
  	| kanye, you are quite beautiful. "2"
  | tina, setup? punchline! "<"
	| kanye, everyone loves you. "5"
	| kanye, you are quite beautiful. "2"
6
```

In Javascript, this looks like

```
if (false || true) {
	3 * 2
} else {
	5 < 2
}
```