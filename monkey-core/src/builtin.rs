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

fn first(args: &[Value]) -> EvaluationResult {
    if args.len() != 1 {
        return Err(EvaluationError(format!("Expected 1 arguments, got {}", args.len())))
    }
    let arg = args.first().unwrap();
    match arg {
        Value::Array(elements) => Ok(elements.first().map_or_else(|| Value::Null, |e| Value::Expression(e.clone()))),
        _ => Err(EvaluationError("Argument to first is not supported".to_string()))
    }
}

fn last(args: &[Value]) -> EvaluationResult {
    if args.len() != 1 {
        return Err(EvaluationError(format!("Expected 1 arguments, got {}", args.len())))
    }
    let arg = args.first().unwrap();
    match arg {
        Value::Array(elements) => Ok(elements.last().map_or_else(|| Value::Null, |e| Value::Expression(e.clone()))),
        _ => Err(EvaluationError("Argument to last is not supported".to_string()))
    }
}

pub(crate) fn get(name: &str) -> Result<BuiltinFunction, EvaluationError> {
    let func = match name {
        "len" => len,
        "first" => first,
        "last" => last,
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

    fn evaluate_code(code: &str) -> Value {
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let env = Environment::new_shared(None);
        program.eval(&env).unwrap()
    }

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
            let return_value = evaluate_code(code);
            assert_eq!(return_value, expectation);
        }
    }

    #[test]
    fn test_first_function_evaluation() {
        let test_cases = vec![
            ("first([])", Value::Null),
            ("first([1])", Value::Expression(Box::new(crate::ast::Integer::new(1)))),
            ("first([1, 2])", Value::Expression(Box::new(crate::ast::Integer::new(1)))),
        ];
        for (source, expectation) in test_cases {
            let return_value = evaluate_code(source);
            assert_eq!(return_value, expectation);
        }
    }

    #[test]
    fn test_last_function_evaluation() {
        let test_cases = vec![
            ("last([])", Value::Null),
            ("last([1])", Value::Expression(Box::new(crate::ast::Integer::new(1)))),
            ("last([1, 2])", Value::Expression(Box::new(crate::ast::Integer::new(2)))),
        ];
        for (source, expectation) in test_cases {
            let return_value = evaluate_code(source);
            assert_eq!(return_value, expectation);
        }
    }
}
