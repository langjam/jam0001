# beatsaber

**beat saber** is a strongly typed, self-documenting and highly performant programming language. With beat saber we aimed to create a language that is incredibly easy to read at first glance and we went above and beyond to fulfill this goal.

Don't believe us? Check it out yourself in the [examples](examples) folder!

## Documentation

See [docs](docs) for more info.

## Prerequisites

* A compatible C compiler
* LLVM 12
* Rust

## Getting Started

See [getting started](docs/getting_started.md)

## Building the compiler

```bash
# Compile to ./target/release/bsc
cargo build --release
# Or alternatively install to ~/.cargo/bin
cargo install --path .
```

## Compiling the example

```bash
# Compile the beatsaber program
bsc examples/beatsaber.beatsaber -o beatsaber
# Run the result
./beatsaber
```

## Credits

* [StackDoubleFlow](https://github.com/StackDoubleFlow)
* [raftario](https://github.com/raftario)
* [Sc2ad](https://github.com/sc2ad)

## Socials

Check out our socials:

[twitter](https://twitter.com/beatsaberbs)

## Disclaimer

<sub>This language inspired by Jaroslav Beck.</sub></br>
<sub><sub>"This language was meant to be used at production scale" -No one ever</sub></sub>
