# honey
The official repository for the Honey programming language.

## examples

### the basics
```ts
// create greeting
let greeting = "Hello";

// The name to print next to "Hello, ". Reassignment of a constant is not allowed.
const name = "World";

// replace the default greeting with a new one
greeting = "Greetings";

// variadic arguments can be passed to @println
@println(greeting, ", ", name, "!");

// generate a random number between 1 and 100
const random = @rand(1, 100);
// string concatenation is also supported
@println("Here is a random number: " + random);
```