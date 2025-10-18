# Brainfuck Interpreter and Optimizing Compiler (Scala)

## Overview
This project implements a complete interpreter and optimizing compiler for the Brainfuck programming language using Scala.  
It demonstrates concepts in compiler design, functional programming, and runtime optimization.  
The system executes Brainfuck code through multiple stages — interpretation, compilation, optimization, and execution — while maintaining clean, recursive, and memory-safe design principles.

---

## Features
- **Interpreter Core:** Executes all Brainfuck instructions (`> < + - . , [ ]`) using recursive evaluation and immutable memory (`Map[Int, Int]`).
- **Jump Table Compilation:** Precomputes loop boundaries (`[` ↔ `]`) to minimize runtime traversal during nested loops.
- **Semantic Optimization:** Simplifies `[-]` patterns to constant-time zero assignments.
- **Instruction Compression:** Merges repeated increment/decrement instructions (e.g., `+++++` → `+5`) to reduce execution overhead.
- **Performance Analysis:** Includes a benchmarking utility (`time_needed()`) to compare optimization levels and runtime improvements.
- **Safe Execution:** Gracefully handles undefined memory cells and supports nested control flow.

---

## Execution Modes

| Mode | Description | Optimization Level | Speed |
|------|--------------|--------------------|--------|
| `run` | Basic interpreter | None | Baseline |
| `run2` | Interpreter with jump table | Medium | +2–3× faster |
| `run3` | Adds semantic optimization (`[-] → 0`) | High | +4–5× faster |
| `run4` | Full optimization and compression | Experimental | Variable |

---

## Usage

### Run the Interpreter
```scala
import M5a._
run(load_bff("hello.bf"))
Run the Optimized Compiler
import M5b._
run3(load_bff("benchmark.bf"))

Measure Performance
import M5b._
time_needed(1, run2(load_bff("mandelbrot.bf")))

Example Program

Hello World

++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]
>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.

Core Concepts Demonstrated

 - Language interpretation and compiler pipeline design

 - Recursive computation and immutable state management

 - Static analysis and code optimization

 - Functional programming principles in Scala

 - Performance benchmarking and runtime evaluation