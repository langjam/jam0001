# some language by some team
The language concept revolves around threads, (each represented internally by a posix thread), which can produce work, as well as commenting on other thread's work! The language is dynamically typed, and compiled to c++!

## Getting started
0. Install clang++ on your system
1. Install nodejs (see https://github.com/nvm-sh/nvm)
2. Run `npm i -g pnpm ts-node` in order to install global dependencies.
3. Run `pnpm i` (in the code subdirectory) in order to install local dependencies
4. (that's a lot of steps) run `ts-node -T src/compiler.ts` while in the `code` subdirectory to use the compiler!

## Samples:
### Hello, world
```rust
@thread
fn main() {
    @log("Hello, world!");
}
```
### A more advanced program with _comments_
```rust
@thread
fn some_thread() {
    let i = 0;
    for {
        let work = @publish_work(i);
        for {
            let comment = @rfc(work);
            if (comment == :main_ack) {
                break;
            }
            @log("Comment: *", comment);
        }
        i += 1;
    }
}

@thread
fn main() {
    for {
        let work = @seework("some_thread");
        @comment(work, :main_ack);
        @log("some_thread's work: *", work);
    }
}
```