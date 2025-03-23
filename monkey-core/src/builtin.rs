use std::result::Result;

use crate::ast::Expr;
use crate::ast::Value;
use crate::ast::ValueIdentity;
use crate::evaluator::EvaluationError;
use crate::evaluator::EvaluationResult;

pub(crate) type BuiltinFunction=fn(args: &[Value]) -> EvaluationResult;

fn assert_argument_length(args: &[Value], expected_arguments_length: usize) -> Result<(), EvaluationError> {
    if args.len() != expected_arguments_length {
        Err(EvaluationError(format!("Expected {expected_arguments_length} arguments, got {}", args.len())))
    } else {
        Ok(())
    }
}

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

fn rest(args: &[Value]) -> EvaluationResult {
    if args.len() != 1 {
        return Err(EvaluationError(format!("Expected 1 arguments, got {}", args.len())))
    }
    let arg = args.first().unwrap();
    if let Value::Array(elements) = arg {
        if elements.is_empty() {
            return Ok(Value::Array(vec![]))
        }
        Ok(Value::Array(elements.clone().split_off(1)))
    } else {
        Err(EvaluationError("Argument to last is not supported".to_string()))
    }
}

fn push(args: &[Value]) -> EvaluationResult {
    assert_argument_length(args, 2)?;
    let arr = args.first().unwrap();
    let new: Expr = Box::new(ValueIdentity::new(args.get(1).unwrap().clone()));
    if let Value::Array(elements) = arr {
        let mut elements = elements.clone();
        elements.push(new);
        Ok(Value::Array(elements))
    } else {
        Err(EvaluationError("Argument to last is not supported".to_string()))
    }
}

pub(crate) fn get(name: &str) -> Result<BuiltinFunction, EvaluationError> {
    let func = match name {
        "len" => len,
        "first" => first,
        "last" => last,
        "rest" => rest,
        "push" => push,
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

        assert!(parser.errors.is_empty());

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

    #[test]
    fn test_rest_function_evaluation() {
        let test_cases = vec![
            ("rest([])", Value::Array(Vec::new())),
            ("rest([1])", Value::Array(Vec::new())),
            ("rest([1, 2])", Value::Array(vec![Box::new(crate::ast::Integer::new(2))])),
        ];
        for (source, expectation) in test_cases {
            let return_value = evaluate_code(source);
            assert_eq!(return_value, expectation);
        }
    }

    #[test]
    fn test_push_function_evaluation() {
        let test_cases = vec![
            ("push([], 1);", Value::Array(vec![Box::new(crate::ast::ValueIdentity::new(Value::Int(1)))])),
            ("let arr = []; push(arr, 1);", Value::Array(vec![Box::new(crate::ast::ValueIdentity::new(Value::Int(1)))])),
        ];
        for (source, expectation) in test_cases {
            let return_value = evaluate_code(source);
            assert_eq!(return_value, expectation);
        }
    }
}
