use std::result::Result;

use crate::ast::Value;
use crate::evaluator::EvaluationError;
use crate::evaluator::EvaluationResult;

pub(crate) type BuiltinFunction=fn(args: &[Value]) -> EvaluationResult;

fn len(args: &[Value]) -> EvaluationResult {
    if args.len() != 1 {
        return Err(EvaluationError(format!("Expected 1 arguments, got {}", args.len())))
    }
    let arg = args.first().unwrap();
    match arg {
        Value::Str(string) => Ok(Value::Int(string.len() as i64)),
        Value::Array(elements) => Ok(Value::Int(elements.len() as i64)),
        _ => Err(EvaluationError("Argument to len not supported".to_string()))
    }
}

pub(crate) fn get(name: &str) -> Result<BuiltinFunction, EvaluationError> {
    let func = match name {
        "len" => len,
        _ => return Err(EvaluationError(format!("{name} is not a known function")))
    };
    Ok(func)
}

#[cfg(test)]
mod tests {
    use crate::ast::Value;
    use crate::environment::Environment;
    use crate::evaluator::Evaluate;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_len_function_evaluation() {
        let test_cases = vec![
            ("len(\"\")", Value::Int(0)),
            ("len(\"four\")", Value::Int(4)),
            ("len(\"hello world\")", Value::Int(11)),
            ("len([])", Value::Int(0)),
            ("len([1])", Value::Int(1)),
            ("len([1, 2])", Value::Int(2)),
        ];

        for (code, expectation) in test_cases {
            let lexer = Lexer::new(code);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            let env = Environment::new_shared(None);
            let return_value = program.eval(&env).unwrap();
            assert_eq!(return_value, expectation);
        }
    }
}
