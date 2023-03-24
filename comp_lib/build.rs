use std::error::Error;
use std::path::Path;
use std::process::Command;

fn main() -> Result<(), Box<dyn Error>> {
    let grammar_in_dir = Path::new("./grammar")
        .canonicalize()
        .expect("./grammar should exist");

    let grammar_in_files = ["CLexer.g4", "CParser.g4"].map(|in_file| {
        let mut in_path = grammar_in_dir.clone();
        in_path.push(in_file);
        if !in_path.exists() {
            panic!(
                "Missing file {} in {}",
                in_path.display(),
                grammar_in_dir.display()
            );
        }
        in_path
    });

    let grammer_out_dir = Path::new("./src/generated")
        .canonicalize()
        .expect("./src/generated should exist");

    let antlr_bin = Path::new("./tools/antlr4-4.8-2-SNAPSHOT-complete-patched.jar")
        .canonicalize()
        .expect("tools/antlr4-4.8-2-SNAPSHOT-complete.jar should exist");

    let status = Command::new("java")
        .current_dir(&grammar_in_dir)
        .arg("-jar")
        .arg(&antlr_bin)
        .args(grammar_in_files)
        .arg("-Werror")
        .arg("-Xexact-output-dir")
        .arg("-Dlanguage=Rust")
        .arg("-o")
        .arg(&grammer_out_dir)
        .status()
        .expect("failed to collect status of antlr");

    if !status.success() {
        panic!("antlr failed");
    }

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", grammar_in_dir.display());

    Ok(())
}
