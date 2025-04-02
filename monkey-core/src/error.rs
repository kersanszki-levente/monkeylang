use std::fmt::Debug;

#[derive(PartialEq)]
struct ErrorMessage(String);

impl Debug for ErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ErrorMessage {
    fn new(source: &str, file: &str, row: usize, col: usize, cause: &str) -> ErrorMessage {
        let mut extract = String::new();
        extract.push_str(&format!("{file}:\n"));
        for (i, line) in source.lines().enumerate() {
            if i == row {
                extract.push_str(&format!("{line}\n{:->col$}^\n", "", col = (col - 1)));
                continue
            }
        }
        ErrorMessage(format!("{extract}\n{cause}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let source = "let a = 1;\nlet b = 2;\na & b";
        let row = 2;
        let col = 3;
        let cause = "Invalid operation between two integers";
        let expectation = ErrorMessage("stdin:\na & b\n--^\n\nInvalid operation between two integers".to_string());
        let result = ErrorMessage::new(source, "stdin", row, col, cause);
        assert_eq!(result, expectation);
    }
}
