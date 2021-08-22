# literally illiterate

This is a [langjam](https://github.com/langjam/langjam) submission (so, this will probably have typos and definitely has shoddy code). 

Play with the REPL [here](https://first-class-comments.netlify.app/).

## What?

[langjam](https://github.com/langjam/langjam) is like a game-jam, but for languages.
Participants are given a theme.
48-hours after they are given the theme, they must write and submit a language that fits this theme.

`langjam001`'s theme was 'first-class comments'.

A language with 'first-class comments' can mean many things.
I chose to interpret it quite literally.
In <strong>literally illiterate</strong>, you send comments to citizens sitting in first class of an airplane.
A comment is not a programming comment (like `//`), but rather a remark.
A valid comment would be, `you dress well`.

Each citizen likes a certain type of comment.
`kanye`, for example, likes compliments.
If you send a comment to a citizen, and they like it, they will evaluate your code.

```
> kanye, you look beautiful tonight "3"
3
```

If they don't like it however, they will ignore you.

## Citizens & Their Likes
- kanye likes compliments
- linus likes to read hackernews headlines
- socrates likes questions
- tina likes a joke with a punchline

## Syntax

`<citizen>, <comment> "<code>"`

## Ownership

Each citizen 'owns' a facet of the language.
So not only do they have to like your comment,
you have to make sure your `<code>`'s type is owned by them.

- kanye is literal (3 'text' true false)
- linus operates (+ - / *)
- tina does comparisons (and or > < <= >=)
- socrates decides (if else)

So, if you send a literal to tina, your literal will not be evaluated.

```
$ tina, setup? punchline! "3"
tina does not own construct "literal"
```

## Chaining

Basic one-liner:

`<citizen>, <comment> "<code>"`

You can also chain statements together to do math

```
$ <citizen>, <comment> "+"
	| <citizen>, <comment> "2"
	| <citizen>, <comment> "3"
```

Or to do an `if else`

```
$ <citizen>, <comment> "if else"
	| <citizen>, <comment> "true"
	| <citizen>, <comment> "5"
	| <citizen>, <comment> "3"
```

The first item in an `if else` is the condition.
The next is the result if the condition is true.
Last is what gets executed if the condition is false.

This can be more complex like so

```
$ <citizen>, <comment> "if else"
	| <citizen>, <comment> "<"
    	| <citizen>, <comment> "5"
    	| <citizen>, <comment> "10"
	| <citizen>, <comment> "*"
    	| <citizen>, <comment> "2"
    	| <citizen>, <comment> "2"
	| <citizen>, <comment> "/"
    	| <citizen>, <comment> "+"
        	| <citizen>, <comment> "4"
        	| <citizen>, <comment> "6"
    	| <citizen>, <comment> "2"
```

If transpiled to javascript, this code would look like this

```
if (5 < 10) {
	2 * 2
} else {
	(4 + 6) / 2
}
```

Both above expressions would result in 5.

## Examples

[See more examples](examples/examples.md)

## Building locally

Check the `README.md` in each sub-directory for notes on running.

```
> deno --version
deno 1.12.2
> node --version
v14.15.5
```

## Notes

- The inference engine (the thing that turns a comment into a label, like `compliment`) can be hit or miss 
as this was the first language model I've trained.
  - (if you also are new, I highly recommend Hugging face. It's extremely easy to get something out the door)
- Compliments are very underrepresented in dataset.