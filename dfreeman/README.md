# Yack

Yack is a language in which documentation comments are first class values, and they themselves may contain and expose other values.

## Building

Yack was developed with Node 16 and Yarn 1.22. In theory it may work with older Node versions and/or recent npm versions. Note that the playground requires a modern browser to run (tested in recent Chrome and Firefox).

To build Yack locally, run the following:

```sh
yarn
yarn dev
```

And then visit http://localhost:3000 in your browser.

Alternatively, assuming I get around to this before judging, I'm planning to publish the playground at https://dfreeman.github.io/langjam-2021.

## The Language

Because much of the point of Yack is the authoring experience and REPL navigation, the playground provides the only currently supported editing environment for Yack. The playground contains a walkthrough of the comment-oriented and more mundane language features in Yack, as well as a few examples of simple algorithms written in the language.

Due to all of the above, you're much better off getting a feel for the language in the playground, but for the sake of completeness, here's the text of one of the toy Yack programs there:

```
###
A list represents an ordered collection of items.

Since Yack is totally dynamically typed, lists can be pretty
free-form. They don't need to contain a consistent type of
element, and they don't even necessarily need a terminating
{Nil()} value (though that's still recommended...)

@example#empty
  Nil()
@end

@example#oneTwoThree
  Cons(1, Cons(2, Cons(3, Nil())))
@end
###
data List = Nil() | Cons(head, tail)

###
This returns the first element of a {List#}, or {void} if
the list is empty.
###
def head = fun(list)
  match list
  | Cons(head, _) -> head
  | _ -> void
  end
end

###
Returns everything BUT the first element of a {List#}, or
{Nil()} if the list is already empty.
###
def tail = fun(list)
  match list
  | Cons(_, tail) -> tail
  | _ -> List#empty
  end
end

###
Reverses the given {List#}, using cute recursion.

For example, if we reverse {List#oneTwoThree}:
@example#oneTwoThree
  reverse(List#oneTwoThree)
@end
We get {reverse#oneTwoThree}
###
def reverse = fun(list)
  (fun reverse(list, reversed)
    match list
    | Nil() -> reversed
    | Cons(head, tail) ->
      reverse(tail, Cons(head, reversed))
    end
  end)(list, Nil())
end
```

And here's a screenshot of an interactive session with that source:

![image](https://user-images.githubusercontent.com/108688/130364807-8e3c9a60-4da6-495d-afff-4680a32b2b8e.png)

<!--
## TODO

Done

- [x] logging/native bindings
- [x] clean up REPL visuals
- [x] ADT instance printing
- [x] rich comment printing
- [x] embedded comment printing
- [x] expression embedding in comments
- [x] example files/walkthrough
- [x] arithemetic
- [x] equality

High priority

- [ ] error reporting in console
- [ ] `@expect` in examples

Lower priority

- [ ] strings
- [ ] error reporting in source
- [ ] tag items with associated docs in repl output

Nice to have

- [ ] modules (nice for more interesting things to do with comments, but unnecessary)
- [ ] REPL history (I keep hitting <kbd>Up</kbd> and expecting it to do something)

-->

```

```

```

```
