# nooga's #langjam2021 entry

## team: **made-up**

-   @nooga - the captain, the ~~helmsman~~ pilot and the ~~ship~~ plane.

It's **made-up** 'cause @jntrnr told me to make up a team name.

## lang: **informal vain**

As in:

-   _I wrote some code in **informal vain**._
-   _Actually, this was not completely written in **informal vain**._
-   _Hey! Have you seen how it's done in **informal vain**._
-   _**Informal vain** attempt to implement persistent data structures in 48h._

#### Why **vain**?

    19:59 <nooga> all this typing in vain

#### Why **informal**?

    19:59 <nooga> I have no proof though

## Building

Prerequisites:

-   `node.js`
-   `npm` or `yarn`

To run all examples:

```
./plswork
```

In case you don't want to run this script:

```
npm install
# or
yarn install
```

Then proceed to run some files:

```
node index.js examples/fib.iv
```

### Examples

They eagerly await your intense scrutiny in the `examples/` folder.

## Motivation

I wanted a language that would have Haskelly syntax but without all this galaxy-brain type related nonsense.

**Informal vain** semantics are close to Javascript so it feels like writing hipster FP-cargo-cult-JS which was the trendiest thing to do for some time... until people discovered that closure allocation is not free.

```
20:01 <nooga> that was precisely my goal.
```

As a Clojure hipster myself I also wanted persistent data structures and a toolbox of functions as clever as Rich Hickey himself.

_cough, didn't write any stdlib, cough_

### Goals

-   [x] Minimal syntax,
-   [x] Compiles to JS,
-   [x] Everything immutable, no assignment,
-   [x] User defined operators,
-   [x] All functions curried by default,
-   [x] Partial application for operators,
-   [ ] Persistent data structures and API à la Clojure,
-   [ ] Pattern matching,
-   [ ] Some interop with JS,
-   [ ] Modules,
-   [ ] REPL
-   [ ] Static compilation to JS,
-   [ ] Unicode identifiers and operators 😵‍💫

### THE Plan

1. Cobble a semi-decent language together by handcrafting everything because I learnt extreme NIH at SerenityOS Inc.,
2. Bolt first-class comments on top of it 20 minutes before the deadline,
3. Tell some jokes in the README,
4. Call VCs, tell more jokes.

```
20:02 <nooga> is there a way to NFT this thing?
```

## First-class comments

> Pre-flight champane and non-offensive crockery included. Enjoy your parse.

```
/*
    multiline comments
    /* can be nested because the lexer has a parser inside :O */
*/

// single line comment
```

Comments in **informal vain** are real comments, as in they can be really placed anywhere in the source and their contents are not subject to any analysis by the compiler.

```clojure
// some func
def foo friends = 404 // arbitrary number
            * /* multiply that by */ 2 // two
            // now we have 808, a cool drum machine, so let's divide it among friends
            / friends;
```

However, each comment is preserved as string and attached to the closest expression according to some made up rules. When the expression is evaluated its value will retain all the attached comments. Computation will just use the values themselves but the comments can be retrieved by calling `RT.E` a value:

```clojure
    def x = 5 + 5; // some x
    x;             // => 10
    RT.E x;        // => "// some x"
```

This brings the comments into the first-class as they can be processed like normal strings using full power of **informal vain**.

Now that I think of it, I should make these comments contain more metadata (line, column, file, etc.) instead of just being strings. Too late though 😑.

## Syntax

Like Haskell but better because no types. If it says _TBD_ below then you'll need to learn the language from examples because I didn't have time to write the docs.

**TBD**

## Remarks

This is not related in any way to #langjam2021 or **informal vain** but I'd like to take a moment to point out that _I personally think Kubernetes is no bueno_.

Hi Mom!
