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

print("==== Testing files")

tests_root = "test_inputs"

tests_out_root = "test_outputs"
if len(sys.argv) >= 2 and sys.argv[1] == "--bless":
    tests_out_root = "test_expected"

if not os.path.exists(tests_out_root):
    print("no output dir, creating it")
    os.makedirs(tests_out_root)
if os.path.isfile(tests_out_root):
    panic(f"{tests_out_root} needs to be a directory")

for file in os.listdir(tests_root):
    full_in_path = os.path.join(tests_root, file)
    full_out_path = os.path.join(tests_out_root, file)
    full_out_path_llvm = os.path.splitext(full_out_path)[0] + ".ll"
    full_out_path_dot = os.path.splitext(full_out_path)[0] + ".dot"

    if not os.path.isfile(full_in_path):
        continue
    print(f"== Testing file: {full_in_path}")
    subprocess.run(
        ["cargo", "run", "--release", "--", full_in_path, "--emit=llvm-ir", "--skip=const-fold", "--output", full_out_path_llvm],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    subprocess.run(
        ["cargo", "run", "--release", "--", full_in_path, "--emit=ast-dot", "--output", full_out_path_dot],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

print("==== Testing diagnostic files")

tests_root = os.path.join(tests_root, "diagnostics")
tests_out_root = os.path.join(tests_out_root, "diagnostics")
if not os.path.exists(tests_out_root):
    print("no diagnostics output dir, creating it")
    os.makedirs(tests_out_root)
if os.path.isfile(tests_out_root):
    panic(f"{tests_out_root} needs to be a directory")

for file in os.listdir(tests_root):
    full_in_path = os.path.join(tests_root, file)
    full_out_path = os.path.join(tests_out_root, file)
    full_out_path = os.path.splitext(full_out_path)[0] + ".txt"

    if not os.path.isfile(full_in_path):
        continue
    print(f"== Testing file: {full_in_path}")
    out_string = subprocess.run(
        ["cargo", "run", "--quiet", "--release", "--", full_in_path],
        stdout=subprocess.DEVNULL, stderr=subprocess.PIPE).stderr
    with open(full_out_path, "wb") as f:
        f.write(out_string)
