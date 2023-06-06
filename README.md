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
`llvm-ir`, `mips-dbg`, `mips-asm`.

```bash
./comp INPUT.c -o OUTPUT.dot -e ast-dot
```

When using one of the `mips` emit option you will also have to set the target to mips
as well. This can be done with the `-t/--target` option (the only two targets are
`mips` and `x86-64`):
```bash
./comp INPUT.c -t mips -e mips-asm
```
`mips-asm` will also be automaticly selected when using the `mips` target.

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
  - `comp_lib/src/codegen`: The generation of llvm/mips code from the ir.
  - `comp_lib/src/structure`: The different trees used by different steps in the
    compilation process.
  - `comp_lib/src/passes`: Code to turn one tree into another.
- `llvm_ir`: Internal library to easily generate llvm.
- `mips_ir`: Internal library to easily generate mips asm and run control flow graph algorithms.

## Operation

1. The source code is parsed into a CST using ANTLR.
1. The CST get transformed into AST.
1. Const folding is run on this AST.
1. The AST gets lowered to a IR, type checking is run at the same time.
1. Some control flow analysis is run on the IR.
1. Depending on the target one of the following is done:

### For the LLVM target:
1. Codegen is run on the IR to create LLVM IR
### For the MIPS target:
The algorithms for register allocation were outlined by Hack et al. (2006) [^1] and to compute liveness sets by Brandner et al. (2011) [^2]
1. Codegen is run on the IR to create an MIPS control flow graph.
1. **Patching**: Insert fake defines where needed.
1. **Dead code elimination**: SSA pruning and removal of unreachable blocks and unused registers.
1. **Register allocation**:
   1. **Call isolation**: Isolate calls in seperate basic blocks.
   1. **Spiling**: Save and load registers to and from memory when out of physical registers.
   1. **Dead code elimination**: Remove new unused registers.
   1. **Call isolation**: Reisolate calls.
   1. **Coloring**: Choose physical registers (colors) for virtual registers.
   1. **Coalesing**: Change colors to minimize moves and swaps.
   1. **SSA destruction**: Replace virtual registers and insert moves.
1. **Stack frame building**.
1. **Devirtualization**: Replace virtual instructions with real MIPS instructions.
1. **Simplification**: Merge linear blocks.
1. **Fixing**: Rearrange blocks and branches to make CFG representable in MIPS asm.
1. **Linking**: Insert premade `printf` and `scanf` when used and add special `main` functionality.

[^1]: Hack, S., Grund, D., & Goos, G. (2006). Register Allocation for Programs in SSA-Form. In Lecture Notes in Computer Science (pp. 247–262). Springer Science+Business Media. https://doi.org/10.1007/11688839_20
[^2]: Brandner, F., Boissinot, B., Darte, A., De Dinechin, B. D., & Rastello, F. (2011). Computing Liveness Sets for SSA-Form Programs. INRIA, 25. https://inria.hal.science/inria-00558509v2

## Running examples

A python script is provided (`run.py`) which will run the compiler on the files in `examples`.
A `.asm` (mips asm), `.llvm`, `.ast.dot`, `.ir.dot`, and `.txt` (compiler output) file will be 
created in the same folder for every input with the same file name. If there's a syntax error,
only the `.txt` file will be generated. If there is a semantic error in the ast to ir step, the
`.ir.dot`, `.asm` and `.llvm` files will not generated, etc.

## Supported features

### Mandatory features

All features required by assignments 1 - 6 are supported. As can be seen in the video.

### Optional features

Almost all optional features suggested by assignments 1 - 6 are also supported.
Only dynmaic arrays and assigning whole array slices to other arrays are unsuported.
See the video for more details

### Extra features not in the assignment

Some things the compiler supports as well, that were not explicitly mentioned
in the assignments.

- Logical `&&` and `||` have short circuiting.
- Assignments can be used in expressions (as specified by the c standard).
- Pointer arithmetic.
- All c types: short, long, unsigned, double.
- Assignments in expressions.
- Bitwise operators: `&`, `|`, `^`, `~`.
- Octal and hexadecimal character escapes in string literals.
- Scientific notation for floats.
- Pointer arithmetic.
- Left and right shift.
- If's, for's, while's with a single statement as body (e.g. `if (...) a = 2;`)
- Octal and hexadecimal number literals.
- Optimized registers allocation

## Dependencies

This project uses the following dependencies:

- `anyhow`: for easier error propagation
- `clap`: a crate to facilitate creating command line interfaces
- `codespan-reporting`: to nicely format the diagnostics
- `is-terminal`: to detect whether stdout is written to a terminal
- `antlr-rust`: fork of ANTLR4 with added support for Rust
- `vec1`: staticly guaranteed non empty vectors
- `generational-arena`: arena based data structures
- `arrayvec`: dynamic array stored on the stack
