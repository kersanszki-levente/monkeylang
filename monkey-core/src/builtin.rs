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
