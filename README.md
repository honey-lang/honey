# honey
honey is a small, embeddable scripting language written in Zig. At its core, honey takes inspiration from languages like Zig, JavaScript/TypeScript, Rust, and Elixir. It's designed to be a simple and easy to use language with a high ceiling for power users. Performance is a key goal of honey, and it's designed to be as fast as possible. Benchmarks will be provided in the future.

## building
honey can be built using this simple command:
```
zig build -Doptimize=ReleaseFast
```

## examples

### accumulator
Like JavaScript, honey supports `let` and `const` bindings. It also supports the continue expression (`i += 1` in this case) from Zig.
```js

const ITERATIONS = 100;

// while loop form
while(i < ITERATIONS): (i += 1) {
    @println("Iterations: ", i);
}

// for loop (exclusive)
for (0..ITERATIONS) |i| {
    let doubled = i * 2;
    @println("Iterations Doubled: ", doubled);
}

// for loop (inclusive)
for (0...ITERATIONS) |i| {
    let tripled = i * 3;
    @println("Iterations Tripled: ", tripled);
}
```