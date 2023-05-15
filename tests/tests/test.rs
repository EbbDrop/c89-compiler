use std::{
    fs,
    io::Write,
    process::{Command, Stdio},
};

use comp_lib::{
    compile::{CompileOptsBuilder, Target},
    diagnostic::{AggregateResult, Code, DiagnosticKind},
};

include! {concat!(env!("OUT_DIR"), "/tests.rs")}

pub fn compile(target: Target, file_name: &str, source: &str) -> AggregateResult<Vec<u8>> {
    let opts = CompileOptsBuilder::new()
        .target(target)
        .output_format(comp_lib::compile::OutputFormat::LlvmIr)
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

fn output_test(file: &str, expected: &str) {
    let source = fs::read(file).unwrap();
    let source = String::from_utf8(source).unwrap();
    let res = compile(Target::X86_64, file, &source);
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
    let llvm = res.into_value().unwrap();
    let output = run_lli(llvm);

    pretty_assertions::assert_str_eq!(
        output.trim_end(),
        expected.trim_end(),
        "The output of lli (left) does not match the expected output (right)",
    );
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
