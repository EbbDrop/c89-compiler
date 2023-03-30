# Comp

A C89 compiler written in ~~Python~~ Rust.

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
output formats are: `ast-dot`, `ast-rust-dbg`, `ir-rust-dbg`, `llvm`.

```bash
./comp INPUT.c -o OUTPUT.dot -e ast-dot
```

Lastly there is also `--skip` to skip some optional passes. At the moment the
only option is `const-fold`. So

```bash
./comp INPUT.c -o OUTPUT.dot -e ast-dot --skip const-fold
```

will not run const folding.

## Project structure

- `comp`: The cli fronted that uses the comp library in `comp_lib`.

- `comp_lib`: The internal library used by the cli to compile files.
  - `comp_lib/grammar`: The ANTLR grammar files.
  - `comp_lib/src/structure`: The different trees used by different steps in the
    compilation process.
  - `comp_lib/src/passes`: Code to turn one tree into another.
- `llvm_ir`: Internal library to easily generate llvm.

## Testing

A python test script is provided (`test.py`) which will run the compiler on the
files in `test_inputs`. A `.llvm` and `.dot` file will be created for every
input in `test_outputs` with the same file name. There are also some files in
`test_inputs/diagnostics`, these are supposed to give a warning or error. For
these files the compiler output is saved instead.

The expected output for all of these files can be found in `test_expected`.

(Note: const-folding is turned off for the llvm files, since most would not
generate any interesting output. This happens because statements that are just a
single constant don't get turned into llvm.)

## Optional features supported

- The `<=`, `>=`, and `!=` comparison operators (`test_inputs/relation_ops.c`)
- The `%` operator (`test_inputs/operators.c`)
- Warning on lossy conversions (`test_inputs/diagnostics/lossy_assign.c`)
- Casts (`test_inputs/casts.c`)
- Comments in LLVM (`test_inputs/comments.c`)
