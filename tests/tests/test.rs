use std::{
    fs::{self},
    io::{Read, Write},
    process::{Command, Stdio},
    time::Duration,
};

use comp_lib::{
    compile::{CompileOptsBuilder, Target},
    diagnostic::{AggregateResult, Code, DiagnosticKind},
};
use temp_file::TempFileBuilder;

include! {concat!(env!("OUT_DIR"), "/tests.rs")}

pub fn compile(target: Target, file_name: &str, source: &str) -> AggregateResult<Vec<u8>> {
    let opts = CompileOptsBuilder::new()
        .target(target)
        .for_assignments()
        .build()
        .unwrap();

    comp_lib::compile::compile(source, file_name, &opts)
}

fn run_lli(input_ir: Vec<u8>) -> String {
    let lli_bin = std::env::var_os("LLI_BIN").unwrap_or_else(|| "lli".into());
    let mut lli = Command::new(lli_bin)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn lli process");

    let mut stdin = lli.stdin.take().expect("Failed to open stdin for lli");
    std::thread::spawn(move || {
        stdin
            .write_all(&input_ir)
            .expect("Failed to write to stdin of lli");
    });

    let output = lli
        .wait_with_output()
        .expect("Failed to read stdout of lli");
    if !output.status.success() {
        println!("lli stderr:\n{}", String::from_utf8_lossy(&output.stderr));
        panic!("lli returned with a non successfull code!");
    }
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn run_mars(input_asm: Vec<u8>) -> String {
    let mars_bin = std::env::var_os("MARS_BIN").unwrap_or_else(|| "mars-mips".into());

    let temp_file = TempFileBuilder::new()
        .suffix(".asm") // Need .asm suffix for mars to work
        .build()
        .unwrap()
        .with_contents(&input_asm)
        .unwrap();

    let mut mars = Command::new(mars_bin)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .arg("nc") // Don't output copyright
        .arg("sm") // Start execution in main label
        .arg(temp_file.path())
        .spawn()
        .expect("Failed to spawn mars process");

    use wait_timeout::ChildExt;
    match mars.wait_timeout(Duration::from_secs(5)) {
        Ok(Some(status)) => {
            if !status.success() {
                panic!("mars returned with a non successfull code!");
            }
        }
        Ok(None) => {
            mars.kill().ok();
            panic!("Hit timeout while running mars")
        }
        Err(_) => panic!("Faild to run mars"),
    }

    let mut output = String::new();
    mars.stdout.unwrap().read_to_string(&mut output).unwrap();

    output
}

fn output_test(file: &str, expected_llvm: &str, expected_mips: &str) {
    for target in [Target::X86_64, Target::Mips] {
        let source = fs::read(file).unwrap();
        let source = String::from_utf8(source).unwrap();
        let res = compile(target, file, &source);
        if res.is_err() {
            println!(
                "Expected file `{}` to compile successfully but got the following diagnostics:",
                file
            );
            for (t, d) in res.diagnostics() {
                match t {
                    DiagnosticKind::Rec => println!("Rec: {d:?}"),
                    DiagnosticKind::Err => println!("Err: {d:?}"),
                }
            }
            panic!();
        }
        let comp_output = res.into_value().unwrap();
        let (output, expected, runner) = if target == Target::X86_64 {
            (run_lli(comp_output), expected_llvm, "lli")
        } else {
            (run_mars(comp_output), expected_mips, "mars")
        };

        pretty_assertions::assert_str_eq!(
            output.trim_end(),
            expected.trim_end(),
            "The output of {runner} (left) does not match the expected output (right)",
        );
    }
}

fn diagnostics_test(file: &str, expected_codes: Vec<Code>, needs_err: bool) {
    for target in [Target::X86_64, Target::Mips] {
        let source = fs::read(file).unwrap();
        let source = String::from_utf8(source).unwrap();
        let res = compile(target, file, &source);
        if needs_err && !res.is_err() {
            panic!("Expected compile to fail, but it didn't!");
        }
        if !needs_err && res.is_err() {
            println!("Expected compile to succeed with only warnings, but it didn't! Here are the error diagnostics:");
            for (t, d) in res.diagnostics() {
                if t == DiagnosticKind::Err {
                    println!("Err: {d:?}")
                }
            }
            panic!("");
        }

        let mut found_code = Vec::new();
        for (_, d) in res.diagnostics() {
            found_code.push(*d.code());
        }
        if expected_codes != found_code {
            println!(
                "Expected to find these diagnostic codes: {:?}
                              But found: {:?}",
                expected_codes, found_code
            );
            panic!("Not the same diagnostics");
        }
    }
}

fn diagnostics_any_test(file: &str, needs_err: bool) {
    for target in [Target::X86_64, Target::Mips] {
        let source = fs::read(file).unwrap();
        let source = String::from_utf8(source).unwrap();
        let res = compile(target, file, &source);
        if needs_err {
            if !res.is_err() {
                panic!("Expected compile to fail, but it didn't!");
            }
        } else if res.is_err() {
            println!("Expected compile to succeed with only warnigs, but it didn't! Here are the error diagnostics:");
            for (t, d) in res.diagnostics() {
                if t == DiagnosticKind::Err {
                    println!("Err: {d:?}")
                }
            }
            panic!("");
        } else if !res.is_rec() {
            panic!("Expected to compile with warnigs, but it didn't!");
        }
    }
}
