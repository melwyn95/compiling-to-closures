# compiling-to-closures

A simple and elegent idea for code-generation

## Paper

Feeley, M., & Lapalme, G. (1987). Using closures for code generation. Computer Languages, 12(1), 47-66.
[Link](https://www.iro.umontreal.ca/~feeley/papers/FeeleyLapalmeCL87.pdf)

## Introduction
- Language implementation: interpreter or compiler
- Interpreter:
  - Easy to implement
  - Better debugging tools
  - Portable
- Compiler
  - Generated code runs faster
- Combine advantages of interpreter and compiler

## Closures
- Scheme: a dialect of Lisp
- In the paper Scheme as source and implementation language
- `lambda` is evaluated to a closure in Scheme
- Using Scheme closures to implement to Scheme compiler in Scheme

## Overview of the compiler
- The compiler targets a base subset of Scheme
  - Higher level constructs `begin`, `cond`, `let`, etc. can be desugared to the base subset
- A closure is generated for each primitive construct of the language
  - When a closure is applied it evaluated the part of original expression 
- A compiled form of an expression combines these closures into a network of closures
- See sections 3.1 to 3.6 for specifics of code generation for each construct
- Some optimizations
  - Can use a **simple list** rather than a assoc-list and use **indexing** to avoid comparisons when looking up vars
  - For commonly occuring constants **generate only one closure** and use to same closure everywhere rather than generating new closure each time
  - Can cache some closures in **LRU-hashtbl** to speed up the compilation process

## Benchmarks

- See section 7 of the paper
- For benchmarks of Haskell & OCaml code [checkout BENCHMARKS.md](./BENCHMARKS.md)