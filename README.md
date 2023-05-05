# Comp

A C89 compiler written in ~~Python~~ Rust.

## Video
_[redacted]_

## Prerequisites

- The Rust compiler, Check [here](https://www.rust-lang.org/tools/install) for
  install instructions.
- Java, Check
  [here](https://www.java.com/en/download/help/download_options.html) for
  install instructions.

## Building and running

To build the compiler run the following in the root directory of this project:

```bash
cargo build --release
```

This creates the executable `./target/release/comp`, which can now be used to
compile files. ANTLR will have been run automatically to create the needed
parser files.

You can also use the following to build and run in one command:

```bash
cargo run --release
```

### Usage

Comp will read from `stdin` by default to get its input. Compiling from a file
is possible by giving it as a positional argument:

```bash
./comp INPUT.c
```

The output will by default also be written to `stdout`, use `-o/--output` to set
an output file path.

```bash
./comp INPUT.c -o OUTPUT.llvm
```

By default llvm ir will be emitted, use `-e/--emit` to change this. The possible
output formats are: `antlr-tree`, `ast-dot`, `ast-rust-dbg`, `ir-rust-dbg`, and
`llvm-ir`.

```bash
./comp INPUT.c -o OUTPUT.dot -e ast-dot
```

Lastly there is also `--skip` to skip some optional passes. The two optional passes are
`const-fold` and `control-flow-analysis`. So

```bash
./comp INPUT.c -o OUTPUT.dot -e ast-dot --skip const-fold
```

will not run const folding.

## Project structure

- `comp`: The cli fronted that uses the comp library in `comp_lib`.

- `comp_lib`: The internal library used by the cli to compile files.
  - `comp_lib/grammar`: The ANTLR grammar files.
  - `comp_lib/src/codegen`: The generation of llvm code from the ir.
  - `comp_lib/src/structure`: The different trees used by different steps in the
    compilation process.
  - `comp_lib/src/passes`: Code to turn one tree into another.
- `llvm_ir`: Internal library to easily generate llvm.

## Running examples

A python script is provided (`run.py`) which will run the compiler on the files in `examples`.
A `.llvm`, `.ast.dot`, `.ir.dot`, and `.txt` (compiler output) file will be created in the same
folder for every input with the same file name. If there's a syntax error, none of these files
will be generated. If there is a semantic error in the ast to ir step, the `.ir.dot` and `.llvm`
files will not generated, etc.

## Supported features

### Mandatory features

All features required by assignments 1 - 6 are supported. This can be seen in the video.
See functionality demonstrated in video.

### Optional features

Almost all optional features suggested by assignments 1 - 6 are also supported.

### Extra features not in the assignment

Some things the compiler supports as well, that were not explicitly mentioned
in the assignments.

- Logical `&&` and `||` have short circuiting.
- Assignments can be used in expressions (as specified by the c standard).
- Pointer arithmetic.
- All c types: short, long, unsigned, double.
- Assignments in expressions.
- Bitwise operators: &, |, ^, ~.
- Octal and hexadecimal character escapes in string literals.
- Scientific notation for floats.
- Pointer arithmetic.
- Left and right shift.
- If's, for's, while's with a single statement as body (e.g. `if (...) continue;`)
- Octal and hexadecimal number literals.

## Dependencies

This project uses the following dependencies:

- `anyhow`: for easier error propagation
- `clap`: a crate to facilitate creating command line interfaces
- `codespan-reporting`: to nicely format the diagnostics
- `is-terminal`: to detect whether stdout is written to a terminal
- `antlr-rust`: fork of ANTLR4 with added support for Rust
