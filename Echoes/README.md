## About
Hello! this is my **very** incomplete entry for LangJam 1!

In this language, you can declare numeric variables, like `:12`!

## Comments
The comments are in first-class seats, you can't interact with them.

## Examples
### Fibonacci
`:1`, `:2`, `:3` and `:n` are variables

```ech
:1 2
:2 1
:3 0
:n 10
loop :n > 0 {
    log :2
    :3  :1 + :2
    :1  :2
    :2  :3
    :n  :n - 1
}
```
### if/else
```ech
if (1 + 2 * 5) = 10 {
    log "Yo this language is so cool!"
} else {
    log "This language is trash"
}
```

## Dependencies
* Linux - you must use Linux to compile and run this program. Tested on Ubuntu 20.04. Not tested with WSL.
* Git - to clone this repository
* GNU Make - to build the project
* GCC - to compile stuff

## Build
To build, write `make`.

To run the interpreter, write `./ech`, and it will
show you a help menu.

## Notes
* It will probably not work as expected sometimes.
* Expect segfaults and stupid stuff.
* This was my first time writing a language!
* I wasn't home for a big chunk of the weekend.
* I will probably continue to develop it, because I like the language's concept.
* Float arithmetic might be broken sometimes.
* A lot of stuff is incomplete :c.
* There is no garbage collector/freeing, so your computer will explode at some point.
* You can check other people's projects, they must be better!
* Routines are incomplete.