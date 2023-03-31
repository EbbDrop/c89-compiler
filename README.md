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
output formats are: `antlr-tree`, `ast-dot`, `ast-rust-dbg`, `ir-rust-dbg`, and
`llvm-ir`.

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

## Supported features

### Mandatory features

All features required by assignments 1 - 3 are supported.

#### Project assignment 1

- Binary operations `+`, `-`, `*`, and `/`. ([test_inputs/operators.c][])
- Binary operations `<`, `<`, and `==`. ([test_inputs/relation_ops.c][])
- Unary operators `+` and `-`. ([test_inputs/operators.c][])
- Brackets to overwrite the order of operations. ([test_inputs/expressions.c][])
- Logical operators `&&`, `||`, and `!`. ([test_inputs/operators.c][],
  [test_inputs/shortcircuiting.c][])
- Const folding. ([test_expected/expressions.dot][])

#### Project assignment 2

- Types: `char`, `float`, `int`, and pointers. ([test_inputs/declarations.c][],
  [test_inputs/pointers.c][])
- Literals of any type. ([test_inputs/declarations.c][])
- Reserved words: `const`, `char`, `float`, `int`. ([test_inputs/declarations.c][])
- Variables: declarations, definitions, and assignments.
  `const` variables are supported as well.
  ([test_inputs/declarations.c][], [test_inputs/assignments.c][])
- Pointer operations: unary `*` and `&`. ([test_inputs/pointers.c][])
- Const propagation. ([test_inputs/const_propagation.c][])
- Syntax errors. ([test_inputs/diagnostics/syntax.c][])
- Semantic errors.
  - Error on undefined variable. ([test_inputs/diagnostics/undef_var.c][])
  - Warn on uninitialized variable. ([test_inputs/diagnostics/warn_on_uninit.c][])
  - Error on redefinition of an existing variable. ([test_inputs/diagnostics/redef_var.c][])
  - Error on incompatible types in operation and assignments.
    ([test_inputs/diagnostics/incompat_types.c][])
  - Error on assignment to a rvalue. ([test_inputs/diagnostics/assign_to_rvalue.c][])
  - Error on assignment to a const variable. ([test_inputs/diagnostics/assign_to_const.c][])

#### Project assignment 3

- Single- and multi-line comments. ([test_inputs/comments.c][])
- Hardcoded `printf` function to output to stdout. ([test_inputs/assignments.c][])
- LLVM IR code generation. (all the examples)

### Optional features

All optional features suggested by assignments 1 - 3 are also supported.

#### Project assignment 1

- Comparison operators `<=`, `>=`, and `!=`. ([test_inputs/relation_ops.c][])
- Binary operator `%`. ([test_inputs/operators.c][])

#### Project assignment 2

- Increment/decrement operations: unary `++` and `--`.
  Both prefix and postfix variants are supported. ([test_inputs/inc_dec.c][])
- Conversions: both implicit, with appriorate warnings on lossy conversions
  ([test_inputs/diagnostics/lossy_assign.c][]), and explicit, using casts
  (cast operator). ([test_inputs/casts.c][])


#### Project assignment 3

- Comments before statements in the original source code are added to the
  generated LLVM as well. ([test_inputs/comments.c][])
- Comments containing the original source code of each statement are also added
  to the generated LLVM. (all examples)

### Extra features not in the assignment

Some things the compiler supports as well, that were not explicitly mentioned
in the assignments.

- Logical `&&` and `||` have short circuiting. ([test_inputs/shortcircuiting.c][])
- Assignments can be used in expressions (as specified by the c standard).
  ([test_inputs/assignments.c][])
- Pointer arithmetic. ([test_inputs/pointer_arith.c][])

## Dependencies

This project uses the following dependencies:

- `anyhow`: for easier error propagation
- `clap`: a crate to facilitate creating command line interfaces
- `codespan-reporting`: to nicely format the diagnostics
- `is-terminal`: to detect whether stdout is written to a terminal
- `antlr-rust`: fork of ANTLR4 with added support for Rust

[test_expected/expressions.dot]: test_expected/expressions.dot
[test_inputs/assignments.c]: test_inputs/assignments.c
[test_inputs/casts.c]: test_inputs/casts.c
[test_inputs/comments.c]: test_inputs/comments.c
[test_inputs/const_propagation.c]: test_inputs/const_propagation.c
[test_inputs/declarations.c]: test_inputs/declarations.c
[test_inputs/diagnostics/assign_to_const.c]: test_inputs/diagnostics/assign_to_const.c
[test_inputs/diagnostics/assign_to_rvalue.c]: test_inputs/diagnostics/assign_to_rvalue.c
[test_inputs/diagnostics/incompat_types.c]: test_inputs/diagnostics/incompat_types.c
[test_inputs/diagnostics/lossy_assign.c]: test_inputs/diagnostics/lossy_assign.c
[test_inputs/diagnostics/redef_var.c]: test_inputs/diagnostics/redef_var.c
[test_inputs/diagnostics/syntax.c]: test_inputs/diagnostics/syntax.c
[test_inputs/diagnostics/undef_var.c]: test_inputs/diagnostics/undef_var.c
[test_inputs/diagnostics/warn_on_uninit.c]: test_inputs/diagnostics/warn_on_uninit.c
[test_inputs/expressions.c]: test_inputs/expressions.c
[test_inputs/inc_dec.c]: test_inputs/inc_dec.c
[test_inputs/operators.c]: test_inputs/operators.c
[test_inputs/pointer_arith.c]: test_inputs/pointer_arith.c
[test_inputs/pointers.c]: test_inputs/pointers.c
[test_inputs/relation_ops.c]: test_inputs/relation_ops.c
[test_inputs/shortcircuiting.c]: test_inputs/shortcircuiting.c
