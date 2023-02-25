use std::error::Error;
use std::path::Path;
use std::process::Command;

fn main() -> Result<(), Box<dyn Error>> {
    let grammer_in_dir = Path::new("./src/parser/grammar")
        .canonicalize()
        .expect("src/parser/grammar should exist");

    let mut grammer_in_file = grammer_in_dir.clone();
    grammer_in_file.push("main.g4");
    if !grammer_in_file.exists() {
        panic!(
            "There should be a main.g4 file in: {}",
            grammer_in_dir.display()
        );
    }

    let grammer_out_dir = Path::new("./src/parser/generated")
        .canonicalize()
        .expect("src/parser/generated should exist");

    let antlr_bin = Path::new("./tools/antlr4-4.8-2-SNAPSHOT-complete.jar")
        .canonicalize()
        .expect("tools/antlr4-4.8-2-SNAPSHOT-complete.jar should exist");

    Command::new("java")
        .current_dir(&grammer_in_dir)
        .arg("-jar")
        .arg(&antlr_bin)
        .arg("main.g4")
        .arg("-Dlanguage=Rust")
        .arg("-o")
        .arg(&grammer_out_dir)
        .spawn()
        .expect("antlr tool failed to start")
        .wait()
        .expect("antlr returned a non 0 error code");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", grammer_in_dir.display());

    Ok(())
}
