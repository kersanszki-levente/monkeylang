type TestResult=Result<(), Box<dyn std::error::Error>>;

const FIBONACCI: &str = "tests/input/fibonacci.mo";

fn run(example_name: &str, expected_file: &str) -> TestResult {
    let expected = std::fs::read_to_string(expected_file)?;
    let output = assert_cmd::Command::cargo_bin("repl")?
        .args(vec!["-c", example_name])
        .output()
        .expect("Failed to execute test");
    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).expect("Invalid UTF-8");
    assert_eq!(stdout, expected);
    Ok(())
}

#[test]
fn test_fibonacci() -> TestResult {
    run(FIBONACCI, "tests/expected/fibonacci.out")
}
