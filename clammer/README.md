# Clammer

Clammer is an experimental JS-inspired interpreted language made to explore the idea of "first-class comments".

## Build instructions

First, run `npm install` to install the dependencies.

To run a clammer .clm file, run: `node index.js run examples/loops.clm`

To run the comments: `node index.js static:tests examples/tests.clm`

## Design

The general thinking behind the language design was based off of how comments often become ways of providing static-time information to developers, _or_ to IDEs. Examples of this are things like JSDoc, TypeScript comment interface, Flow, Python type hint comments. The problem with these is that the extensions to the language usually end up being plain text, since they live in comments. Even though eg JSDoc does have a defined grammar and structure, unless your IDE supports it or you're using the flow compiler, you can make errors.

I wanted to try adding syntax to comments, to allow for easy expansion of static time checks. One such example was adding unit tests:

```clm
# tests([
    square(3) == 9,
    square(4) == 12
])
square = function(x) { x^2 };
```

Here, when the code is run normally, the comment is entirely ignored. It has no runtime impact (just like normal comments).

But when the code is run in static mode:

```
$ node index.js static:tests examples/tests.clm
square: ✅ ❌ 
❌ square(4) == 12
is_even: ✅ ❌ ✅
❌ is_even(1) == true
sum_range: ❌
❌ sum_range(0, 5, 0) == 15
```

The code inside the comments is also executed. And in fact, `tests` is just a normal function (See it in examples/tests.clm). Static code (code in comments) can access runtime code, but not the other way around.

Of course, normal text-only comments still work, but are now strings:

```clm
# "
    Loads and loads of comments...
    And some more comments...
"
types = function() { ... }
```

### How it works

The way this works, is `tests` is a function-generating function. Similar to a decorator, that is given a "block" object. This block object provides low-level access to compiler input (like file location of values, etc), allowing the `tests` function to provide specific input about where something went wrong. Or at least that was the intention anyways; right now it does a very small fraction of any of that! And a lot of the information that's only needed for this static-time run is also included in the normal run, which it shouldn't be.

### Related ideas

Things that I thought this might be useful for


- Static type type checking
    ```clm
    # types({ in: [int], out: int })
    square = function (x) { x^2 }
    ```
- Label auto-generated code and prevent it from going stale
    ```clm
    # generated_by(other_fun)
    letters = ["A", ...]
    ```
- Documentation of course
    ```clm
    # docs("
        This function does something or other
    ")
    my_fn = function() { ... }
    ```
- Debugging print statements that never run at runtime
    ```clm
    forEach(arr, function(x) {
        # print("X is " + x)
        ...
    })
    ```
- Add support for static-checking of variable preconditions
    ```clm
    # preconditions([
        function(start, end) { start < end }
    ])
    range = function (start, end) { ... }
    ```

### Problems/Missing

Things I ran out of time to fix/implement

- Array access -_-
- API for querying the code base to apply eg type checking
- Ability to add comments not just before expressions
- Return statement
- No semicolons (they're quit annoying!)