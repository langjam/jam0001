## About
Hello! this is my entry for LangJam 1!
*5 minute Whale sounds*

## Example
### Fibonacci
```ech
:a 2
:b 1
:c 0
:n 10
loop :n > 0 {
    log :b
    :c  :a + :b
    :a  :b
    :b  :c
    :n  :n - 1
}
```

## Dependencies
* Git - to clone this repository
* GNU Make - to build the project
* GCC - to compile stuff

## Build
To build, write `make`.
To run the interpreter, write `./ech`, and it will
show you a help menu.

## Notes
* Float arithmetic might be broken sometimes.
