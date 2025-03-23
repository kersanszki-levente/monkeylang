# Monkeylang

An implementation of monkey based on the book [Writing an Interpreter in Go](https://interpreterbook.com/) written by [Thorsten Ball](https://thorstenball.com/). This implementation is written in Rust and it is not considered complete. Currently implemented features:

* Primitive types: bools and integers.
* Complex types: arrays.
* Prefix (!, -) and infix (+, -, *, /, &, |, >, <) operators.
* Storing expression results as variables.
* Functions, closures and recursion.
* Built-in functions: `len`, `first`, `last`, `rest`, `push`.

## REPL

The language comes with a repl that can be built with [cargo](https://rustup.rs/) by invoking the `make repl` command in the command line.
