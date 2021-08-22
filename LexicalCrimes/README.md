<div align="center">
    <h1><code>regretti</code> <img src="https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/google/298/pinched-fingers_1f90c.png" height="32" width="32" alt="github doesn't have this emoji :(">🍝</h1>
</div>

<p align="center">
<strong><i>A programming language where comments are the first-class citizen and ASCII art flowcharts are the controls!</i></strong>
</p>

---

> Made for [Lang Jam (`jam0001`)](https://github.com/langjam/jam0001) theme: **first-class comments** in 48 hours.

## Summary

What if all those "`TODO:`" comments and beautiful ASCII art diagrams in your code actually worked? Presenting you the one and only _regretti_, write your _spaghetti_ and _forgetti_!

> _Basically, I took the theme "literally"!_

So comments are **commands**!

You can only run instruction statements inside a comment! 💬

## Table of Contents

* [Features](#goals)
* [Installation](#installation)
* [Quick Start](#hello-world)
* [Examples](#examples)
* [Language Internals](#internals)

## Goals

- **Teaching:** What initially started as a troll/esoteric language, it language could be used to teach kids "Control Flows" visually.
- **High-performance:** The existing functionality/examples runs faster than most interpreted languages!
- **Helpful Error Messages:** Almost every errors will be catched and prints out a helpful error message as to denote what went wrong. (Inspired from Rust :heart:)

## Installation

```
$ git clone https://github.com/mufeedvh/regretti.git
$ cd regretti/
$ cargo build --release
$ ./target/release/regretti
```

## Hello World

Lo and behold!

```
main:
    /*
        +----------------------+
        | print "Hello World!" |
        +----------------------+
    */
```

**Loops**

```

main:
    let count = 69420

    /*
        +--------+
        | loop 5 |<----------+
        +--------+           |
             |               v
             |               +-----------------------+
             |               | print count           |
             |               +-----------------------+
             |               ^
             |               |
             +---------------+
    */
:end
```

Get more examples below!

## Examples

Examples are the best way to learn regretti, these cover all the features and functionalities of the language:

- [Hello World](https://github.com/mufeedvh/regretti/blob/main/examples/helloworld.reg)
- [If Else](https://github.com/mufeedvh/regretti/blob/main/examples/if_else.reg)
- [Loops](https://github.com/mufeedvh/regretti/blob/main/examples/loop.reg)

_All other examples have not been completely implemented yet._

_Also I am pretty sure there are a lot of bugs, this is probably the most obscure syntax for parsing lol..._

### Flow Charts?

<div align="center">
<img src="https://imgs.xkcd.com/comics/flow_charts.png" height="400" width="600">
</div>

## Internals

**In a nutshell:**

<img src="https://imgs.xkcd.com/comics/encoding.png" height="350" width="250">

_https://xkcd.com/1209/_

**NOTE:** I couldn't implement the language completely (all available examples works correctly, run `tests` for more) because I ran out of time (I was kinda busy during the time). Thank You for checking out a 1700 line spaghetti. :heart::raised_hands:
