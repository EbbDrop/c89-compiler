use std::{
    env,
    error::Error,
    ffi::OsStr,
    fs::{self, File},
    io::{self, BufRead},
    path::Path,
};
use walkdir::WalkDir;

enum Test {
    Output(String, String),
    Diagnostics(Vec<String>, bool),
    DiagnosticsAny(bool),
}

fn parse_file<P>(filename: P) -> Option<(Test, bool)>
where
    P: AsRef<Path>,
{
    let file = File::open(&filename)
        .unwrap_or_else(|_| panic!("Failed to open file {}", filename.as_ref().display()));
    let mut lines = io::BufReader::new(file).lines();
    let first_line = lines.next().expect("Empty test file").unwrap();

    let (first_line, ignore) = if first_line == "//ignore" {
        (lines.next().expect("Empty test file").unwrap(), true)
    } else {
        (first_line, false)
    };

    let tests = match first_line.as_str() {
        "//output:" => {
            let mut expected = String::new();
            let mut expected_mips = String::new();

            let mut adding_to_mips = false;

            while let Some(Ok(line)) = lines.next() {
                if let Some(expect) = line.strip_prefix("//") {
                    if expect == "output-mips:" {
                        adding_to_mips = true;
                        continue;
                    }
                    match adding_to_mips {
                        true => {
                            expected_mips.push_str(expect);
                            expected_mips.push('\n');
                        }
                        false => {
                            expected.push_str(expect);
                            expected.push('\n');
                        }
                    }
                } else {
                    break;
                }
            }
            if !adding_to_mips {
                expected_mips = expected.clone();
            }
            Some(Test::Output(expected, expected_mips))
        }
        "//fail:" | "//warn:" => {
            let mut expected_codes = Vec::new();
            while let Some(Ok(line)) = lines.next() {
                if let Some(expect) = line.strip_prefix("//") {
                    expected_codes.push(expect.to_owned());
                } else {
                    break;
                }
            }
            Some(Test::Diagnostics(expected_codes, first_line == "//fail:"))
        }
        "//fail-any:" => Some(Test::DiagnosticsAny(true)),
        "//warn-any:" => Some(Test::DiagnosticsAny(false)),
        _ => {
            println!(
                "cargo:warning=Failed to read test file `{}` starting with {}",
                filename.as_ref().display(),
                first_line
            );
            None
        }
    };

    tests.map(|t| (t, ignore))
}

fn make_save(name: &OsStr) -> String {
    let name = name.to_string_lossy();

    let mut out = String::new();
    for c in name.chars() {
        if c.is_ascii_alphanumeric() {
            if c.is_ascii_uppercase() {
                out.push('_');
            }
            out.push(c.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    out
}

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let input_dir = "test_files";

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", input_dir);
    let mut output = String::new();

    let mut i = 0;
    for entry in WalkDir::new(input_dir).into_iter().filter_map(|e| e.ok()) {
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        if let Some((test, ignore)) = parse_file(path) {
            let test = match test {
                Test::Output(expected, expected_mips) => {
                    format!(
                        r#"output_test("{}", "{}", "{}")"#,
                        path.display(),
                        expected,
                        expected_mips
                    )
                }
                Test::Diagnostics(expected_codes, need_err) => {
                    format!(
                        r#"diagnostics_test("{}", vec![{}], {})"#,
                        path.display(),
                        expected_codes
                            .iter()
                            .map(|c| "Code::".to_owned() + c)
                            .collect::<Vec<_>>()
                            .join(", "),
                        need_err
                    )
                }
                Test::DiagnosticsAny(need_err) => {
                    format!(
                        r#"diagnostics_any_test("{}", {})"#,
                        path.display(),
                        need_err
                    )
                }
            };

            let test_name = path.strip_prefix(input_dir).unwrap();
            let test_name = make_save(test_name.as_os_str());

            let test_specifier = match ignore {
                true => "#[test]\n#[ignore]",
                false => "#[test]",
            };

            let test = format!(
                "{}\nfn {}_{}() {{\n{};\n}}\n",
                test_specifier, test_name, i, test
            );

            output.push_str(&test);
            i += 1;
        }
    }

    fs::write(out_dir + "/tests.rs", output).expect("Failed to write to tests.rs");

    Ok(())
}
