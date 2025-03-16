use core::result::Result;
use core::fmt::Debug;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Boolean;
use crate::ast::CallExpression;
use crate::ast::Expression;
use crate::ast::FunctionLiteral;
use crate::ast::Identifier;
use crate::ast::IfExpr;
use crate::ast::InfixExpr;
use crate::ast::Integer;
use crate::ast::PrefixedExpr;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::StringLiteral;
use crate::ast::Value;
use crate::environment::Environment;
use crate::environment::SharedEnvironment;
use crate::token::TokenType;

pub struct EvaluationError(String);

impl Debug for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

type EvaluationResult=Result<Value, EvaluationError>;

pub trait Evaluate {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult;
}

impl Evaluate for Program {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        let mut result = Value::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval(env)?;
            if let Value::Return(value) = result {
                return Ok(Value::Return(value))
            } else {
                continue
            }
        };
        Ok(result)
    }
}

impl Evaluate for Statement {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        match self {
            Statement::Let(id, expr) => {
                let value = expr.eval(env)?;
                let id = id.to_string();
                env
                    .try_borrow_mut()
                    .map_err(|_| EvaluationError("Compiler error".to_string()))?
                    .set(id, value);
                Ok(Value::Null)
            },
            Statement::Return(expr) => Ok(Value::Return(Box::new(expr.eval(env)?))),
            Statement::Expression(expr) => expr.eval(env),
            Statement::Block(exprs) => {
                let mut values = exprs
                    .iter()
                    .map(|expr| -> EvaluationResult { expr.eval(env) })
                    .peekable();
                if let Some(return_value) = values.next_if(is_return_statement) {
                    return return_value
                };
                if let Some(result) = values.last() {
                    result
                } else {
                    Ok(Value::Null)
                }
            }
        }
    }
}

fn is_return_statement(result: &EvaluationResult) -> bool {
    match result {
        Ok(value) => matches!(value, Value::Return(_)),
        _ => false
    }
}

impl Evaluate for Value {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        match self {
            Value::Return(expr) => expr.eval(env),
            Value::Expression(expr) => expr.eval(env),
            v => Ok(v.clone())
        }
    }
}

impl Evaluate for Integer {
    fn eval(&self, _: &SharedEnvironment) -> EvaluationResult {
        Ok(self.value())
    }
}

impl Evaluate for StringLiteral {
    fn eval(&self, _: &SharedEnvironment) -> EvaluationResult {
        Ok(self.value())
    }
}

impl Evaluate for Boolean {
    fn eval(&self, _: &SharedEnvironment) -> EvaluationResult {
        Ok(self.value())
    }
}

impl Evaluate for InfixExpr {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        let left_value = match &self.left {
            Value::Expression(left_expr) => left_expr.eval(env)?,
            _ => self.left.clone(),
        };
        let right_value = match &self.right {
            Value::Expression(right_expr) => right_expr.eval(env)?,
            _ => self.right.clone(),
        };
        match left_value {
            Value::Int(left) => {
                match right_value {
                    Value::Int(right) => {
                        match self.operator {
                            TokenType::Plus => Ok(Value::Int(left + right)),
                            TokenType::Minus => Ok(Value::Int(left - right)),
                            TokenType::Asterisk => Ok(Value::Int(left * right)),
                            TokenType::Slash => Ok(Value::Int(left / right)),
                            TokenType::Gt => Ok(Value::Bool(left > right)),
                            TokenType::Lt => Ok(Value::Bool(left < right)),
                            TokenType::Eq => Ok(Value::Bool(left == right)),
                            TokenType::NotEq => Ok(Value::Bool(left != right)),
                            TokenType::LtE => Ok(Value::Bool(left <= right)),
                            TokenType::GtE => Ok(Value::Bool(left >= right)),
                            _ => Err(EvaluationError(format!("{} is not a valid operator for two integers", self.operator)))
                        }
                    },
                    _ => Err(EvaluationError("Cannot mix integers with other types".to_string()))
                }
            },
            Value::Bool(left) => {
                match right_value {
                    Value::Bool(right) => {
                        match self.operator {
                            TokenType::Eq => Ok(Value::Bool(left == right)),
                            TokenType::NotEq => Ok(Value::Bool(left != right)),
                            _ => Err(EvaluationError(format!("{} is not a valid operator for two booleans", self.operator)))
                        }
                    },
                    _ => Err(EvaluationError("Cannot mix bools with other types".to_string()))
                }
            },
            Value::Str(left) => {
                match right_value {
                    Value::Str(right) => {
                        match self.operator {
                            TokenType::Plus => Ok(Value::Str(format!("{left}{right}"))),
                            _ => Err(EvaluationError(format!("{} is not a valid operator for two strings", self.operator)))
                        }
                    },
                    _ => Err(EvaluationError("Cannot mix strings with other types".to_string()))
                }
            }
            _ => unimplemented!()
        }
    }
}

impl Evaluate for Identifier {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        let id = self.literal();
        env
            .try_borrow()
            .map_err(|_| EvaluationError("Compiler error".to_string()))?
            .get(id)
            .map_err(|_| EvaluationError(format!("{} is not a variable", self.literal())))
    }
}

impl Evaluate for PrefixedExpr {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        let right = self.right.eval(env)?;
        match self.operator {
            TokenType::Bang => eval_bang_operator(&right),
            TokenType::Minus => eval_minus_operator(&right),
            _ => Err(EvaluationError(format!("Unknown prefix operator {}", self.operator)))
        }
        
    }
}

fn eval_bang_operator(value: &Value) -> EvaluationResult {
    let outcome = match value {
        Value::Bool(inner_value) => !(*inner_value),
        Value::Null => true,
        _ => false,
    };
    Ok(Value::Bool(outcome))
}

fn eval_minus_operator(value: &Value) -> EvaluationResult {
    match value {
        Value::Int(inner_value) => Ok(Value::Int(-inner_value)),
        _ => Err(EvaluationError(format!("{} is not an integer", value)))
    }
}

impl Evaluate for IfExpr {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        let condition_value = self.condition.eval(env)?;
        if is_condition_true(condition_value, env)? {
            self.consequence.eval(env)
        } else if let Some(alternative) = self.alternative.as_ref() {
            alternative.eval(env)
        } else {
            Ok(Value::Null)
        }
    }
}

fn is_condition_true(condition_value: Value, env: &SharedEnvironment) -> core::result::Result<bool, EvaluationError> {
    let value = match condition_value {
        Value::Expression(expr) => expr.eval(env)?,
        _ => condition_value,
    };
    match value {
        Value::Null => Ok(false),
        Value::Bool(inner) => Ok(inner),
        _ => Ok(true),
    }
}

impl Evaluate for FunctionLiteral {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        Ok(Value::Function(Box::new(self.body.clone()), Box::new(self.parameters.clone()), env.clone()))
    }
}

impl Evaluate for CallExpression {
    fn eval(&self, env: &SharedEnvironment) -> EvaluationResult {
        let function = self.function.eval(env)?;
        let args = self
            .arguments
            .iter()
            .map(|arg| arg.eval(env))
            .collect::<Result<Vec<Value>, EvaluationError>>()?;
        apply_function(&function, &args)
    }
}

fn apply_function(func: &Value, args: &[Value]) -> EvaluationResult {
    match func {
        f @ Value::Function(stmt, _, _) => {
            let function_env = extend_function_env(f, args)?;
            let value = stmt.eval(&function_env)?;
            unwrap_return_value(&value)
        },
        _ => Err(EvaluationError(format!("Evaluation error: {func} is not callable")))
    }
}

fn extend_function_env(func: &Value, args: &[Value]) -> std::result::Result<SharedEnvironment, EvaluationError> {
    if let Value::Function(_, parameters, func_env) = func {
        let mut function_env = Environment::new(Some(Rc::downgrade(func_env)));
        for (parameter, argument) in parameters.iter().zip(args.iter()) {
            function_env.set(parameter.literal().to_string(), argument.clone());
        }
        Ok(Rc::new(RefCell::new(function_env)))
    } else {
        Err(EvaluationError("Value is not a function".to_string()))
    }
}

fn unwrap_return_value(value: &Value) -> EvaluationResult {
    if let Value::Return(expr) = value {
        Ok(*expr.clone())
    } else {
        Ok(value.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{ast::Expr, lexer::Lexer, parser::Parser};

    #[test]
    fn test_program_evaluation() {
        let left_int: Expr = Box::new(Integer::new(3));
        let right_int: Expr = Box::new(Integer::new(7));
        let one = Box::new(InfixExpr::new(TokenType::Plus, &left_int, &right_int));
        let statement = Statement::Expression(one);
        let mut program = Program::new();
        program.statements.push(statement);

        let expected = Value::Int(10);
        let mut env = Environment::new_shared(None);

        assert_eq!(program.eval(&mut env).unwrap(), expected);
    }

    #[test]
    fn test_expression_statement_evaluation() {
        let left_int: Expr = Box::new(Integer::new(3));
        let right_int: Expr = Box::new(Integer::new(7));
        let one = Box::new(InfixExpr::new(TokenType::Plus, &left_int, &right_int));
        let statement = Statement::Expression(one);

        let expected = Value::Int(10);
        let mut env = Environment::new_shared(None);

        assert_eq!(statement.eval(&mut env).unwrap(), expected);
    }

    #[test]
    fn test_prefix_expression_evaluation() {
        let test_cases = vec![
            ("!true", Value::Bool(false)),
            ("!false", Value::Bool(true)),
            ("!3", Value::Bool(false)),
            ("!!true", Value::Bool(true)),
            ("!!false", Value::Bool(false)),
            ("!!5", Value::Bool(true)),
        ];

        for (func_input, expected_value) in test_cases {
            let lexer = Lexer::new(func_input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();

            let env = Environment::new_shared(None);

            assert_eq!(program.eval(&env).unwrap(), expected_value);
        }
    }

    #[test]
    fn test_infix_expression_evaluation() {
        let test_cases = vec![
            ("5", Value::Int(5)),
            ("-5", Value::Int(-5)),
            ("5 + 5", Value::Int(10)),
            ("5 - 5", Value::Int(0)),
            ("5 / 5", Value::Int(1)),
            ("5 * 5", Value::Int(25)),
            ("5 + 5 + 5 + 5 - 10", Value::Int(10)),
            ("2 * 2 * 2 * 2 *2", Value::Int(32)),
            ("-50 + 100 + -50", Value::Int(0)),
            ("5 * 2 + 10", Value::Int(20)),
            ("5 + 2 * 10", Value::Int(25)),
            ("20 + 2 * -10", Value::Int(0)),
            ("50 / 2 * 2 + 10", Value::Int(60)),
            ("2 * (5 + 10)", Value::Int(30)),
            ("3 * 3 * 3 + 10", Value::Int(37)),
            ("3 * (3 * 3) + 10", Value::Int(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Value::Int(50)),
            ("5 > 5", Value::Bool(false)),
            ("5 < 5", Value::Bool(false)),
            ("5 == 5", Value::Bool(true)),
            ("5 != 5", Value::Bool(false)),
            ("10 >= 10", Value::Bool(true)),
            ("10 >= 9", Value::Bool(true)),
            ("9 >= 10", Value::Bool(false)),
            ("10 <= 10", Value::Bool(true)),
            ("10 <= 9", Value::Bool(false)),
            ("9 <= 10", Value::Bool(true)),
            ("let t = fn() { return true }; t() == true", Value::Bool(true)),
            ("let t = fn() { return true }; t() != true", Value::Bool(false)),
        ];

        for (func_input, expected_value) in test_cases {
            let lexer = Lexer::new(func_input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            let env = Environment::new_shared(None);

            assert_eq!(program.eval(&env).unwrap(), expected_value);
            println!("{func_input}");
        }
    }

    #[test]
    fn test_if_expression_evaluation() {
         let test_cases = vec![
            ("if (true) { 10 }", Value::Int(10)),
            ("if (false) { 10 }", Value::Null),
            ("if (1) { 10 }", Value::Int(10)),
            ("if (1 < 2) { 10 }", Value::Int(10)),
            ("if (1 > 2) { 10 }", Value::Null),
            ("if (1 < 2) { 10 } else { 20 }", Value::Int(10)),
            ("if (1 > 2) { 10 } else { 20 }", Value::Int(20)),
        ];

        for (func_input, expected_value) in test_cases {
            let lexer = Lexer::new(func_input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            let env = Environment::new_shared(None);

            assert_eq!(program.eval(&env).unwrap(), expected_value);
        }
    }

    #[test]
    fn test_return_statement_evaluation() {
        let test_cases = vec![
            ("return 10;", Value::Return(Box::new(Value::Int(10)))),
            ("return 10; 9;", Value::Return(Box::new(Value::Int(10)))),
            ("return 2 * 5; 9;", Value::Return(Box::new(Value::Int(10)))),
            ("9; return 2 * 5; 9;", Value::Return(Box::new(Value::Int(10)))),
        ];

        for (func_input, expectation) in test_cases {
            let lexer = Lexer::new(func_input);
            let env = Environment::new_shared(None);
            let program = Parser::new(lexer).parse().eval(&env).unwrap();
            assert_eq!(program, expectation);
        }
    }

    #[test]
    fn test_let_statement_evaluation() {
        let test_cases = vec![
            ("let a = 5; a;", Value::Int(5)),
            ("let a = 5 * 5; a;", Value::Int(25)),
            ("let a = 5; let b = a; b;", Value::Int(5)),
            ("let a = 5; let b = a; let c = a + b; c", Value::Int(10)),
        ];

        for (func_input, expectation) in test_cases {
            let lexer = Lexer::new(func_input);
            let env = Environment::new_shared(None);
            let program = Parser::new(lexer).parse().eval(&env).unwrap();
            assert_eq!(program, expectation);
        }
    }

    #[test]
    fn test_function_literal_evaluation() {

        let func_input = "fn(x) { x + 2 };";
        let lexer = Lexer::new(func_input);
        let env = Environment::new_shared(None);
        let program = Parser::new(lexer).parse().eval(&env).unwrap();

        let offset_value: Expr = Box::new(Integer::new(2));
        let func_var: Expr = Box::new(Identifier::new("x"));
        let body = Statement::Expression(Box::new(InfixExpr::new(TokenType::Plus, &func_var, &offset_value)));
        let expectation = Value::Function(Box::new(Statement::Block(vec![body])), Box::new(vec![Identifier::new("x")]), env);
        assert_eq!(program, expectation);
    }

    #[test]
    fn test_call_expression_evaluation() {
        let test_cases = vec![
            ("fn() { return 1 }()", Value::Int(1)),
            ("let echo = fn(word) { return word }; echo(6);", Value::Int(6)),
            ("let sum = fn(a, b) { return a + b }; sum(1, 5)", Value::Int(6)),
            ("let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(4)", Value::Int(6)),
        ];

        for (func_input, expected_return_value) in test_cases {
            let lexer = Lexer::new(func_input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            assert!(parser.errors.is_empty());

            let env = Environment::new_shared(None);
            let return_value = program.eval(&env).unwrap();
            assert_eq!(return_value, expected_return_value);
        }
    }

    #[test]
    fn test_recursive_evaluation() {
        let code = "let fib = fn(n) { if (n <= 1) { return n } else { return fib(n-1) + fib(n-2)} }; fib(5)";
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert!(parser.errors.is_empty());

        let env = Environment::new_shared(None);
        let return_value = program.eval(&env).unwrap();
        assert_eq!(return_value, Value::Int(5));
    }

    #[test]
    fn test_global_variable_evaluation() {
        let code = "let word = 1; let process = fn() { return word }; process()";
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let env = Environment::new_shared(None);
        let return_value = program.eval(&env).unwrap();
        assert_eq!(return_value, Value::Int(1));
    }

    #[test]
    fn test_string_concatenation_evaluation() {
        let code = "\"Hello\" + \" \" + \"World\"";
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let env = Environment::new_shared(None);
        let return_value = program.eval(&env).unwrap();
        assert_eq!(return_value, Value::Str("Hello World".to_string()));
    }
}
