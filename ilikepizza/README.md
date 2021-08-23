# `toggler`

A for-fun esoteric programming language that uses comments as its control flow (no goto/jump)

```sh
add 5 7
uncomment-range 2 2
#5 + 7 = %
#print-format
```

## Building & Running

You will need to [install the Rust toolchain](https://www.rust-lang.org/tools/install)

```sh
cargo build --release
target/release/toggler --help
```

## Examples

Examples can be found in the `examples` directory

Note: If you can manage to write a fibonacci program, please send it to n.darazaki@gmail.com
(I tried to do it but gave up and am really curious if that's possible. I can send a rare pic of my cat in return!)

## Documentation

Because of the lack of time I couldn't write it here so you can check the `exec_line` function in `src/main.rs`.

Sorry :(

