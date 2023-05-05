# This script builds the compiler and runs it on the files in `test_inputs`. Cargo and java need
# to be installed for this to work.

import subprocess
import os
import sys


def panic(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
    sys.exit(1)


print("==== Building compiler")
# Compiler comp
if subprocess.run(["cargo", "build", "--release"]).returncode != 0:
    panic("\n\nFailed to build the compiler!!")

print("==== Running files")

examples_root = "examples"

if os.path.isfile(examples_root):
    panic(f"{examples_root} needs to be a directory")

for (
    root,
    dirs,
    files,
) in os.walk(examples_root):
    for name in files:
        full_in_path = os.path.join(root, name)

        full_out_path = os.path.join(root, name)
        full_out_path_llvm = os.path.splitext(full_out_path)[0] + ".ll"
        full_out_path_ast_dot = os.path.splitext(full_out_path)[0] + ".ast.dot"
        full_out_path_ir_dot = os.path.splitext(full_out_path)[0] + ".ir.dot"
        full_out_path_text = os.path.splitext(full_out_path)[0] + ".txt"

        if not os.path.isfile(full_in_path):
            continue
        print(f"== Testing file: {full_in_path}")
        subprocess.run(
            [
                "cargo",
                "run",
                "--release",
                "--",
                full_in_path,
                "--emit=llvm-ir",
                "--output",
                full_out_path_llvm,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        subprocess.run(
            [
                "cargo",
                "run",
                "--release",
                "--",
                full_in_path,
                "--emit=ast-dot",
                "--output",
                full_out_path_ast_dot,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        subprocess.run(
            [
                "cargo",
                "run",
                "--release",
                "--",
                full_in_path,
                "--emit=ir-dot",
                "--output",
                full_out_path_ir_dot,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        out_string = subprocess.run(
            ["cargo", "run", "--quiet", "--release", "--", full_in_path],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        ).stderr
        if out_string != "":
            with open(full_out_path_text, "wb") as f:
                f.write(out_string)
