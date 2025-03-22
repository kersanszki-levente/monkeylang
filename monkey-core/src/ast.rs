use core::fmt::Debug;
use core::fmt::Display;
use std::ops::Deref;

use crate::environment::SharedEnvironment;
use crate::evaluator::Evaluate;
use crate::token::TokenType;

pub trait Expression: Display + Debug + Evaluate {
    fn value(&self) -> Value;
    fn token_type(&self) -> TokenType;
    fn literal(&self) -> &str;
    fn box_clone(&self) -> Box<dyn Expression>;
}

pub(crate) type Expr=Box<dyn Expression>;

impl Clone for Expr {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.token_type() == other.token_type() &&
        self.literal() == other.literal()
    }
}

impl Eq for Expr {
    fn assert_receiver_is_total_eq(&self) {}
}

#[derive(Debug, Clone)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Expression(Expr),
    Return(Box<Value>),
    Function(Box<Statement>, Box<Vec<Identifier>>, SharedEnvironment),
    Array(Vec<Expr>),
    Null,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self {
            Value::Str(value) => value,
            Value::Int(value) => &format!("{value}"),
            Value::Bool(value) => &format!("{value}"),
            Value::Expression(expr) => &format!("{expr}"),
            Value::Return(expr) => &format!("{expr}"),
            Value::Function(call_expr, _, _) => &format!("{call_expr}"),
            Value::Array(elements) => {
                &format!("[{}]", elements.iter().map(|e| format!("{e}")).collect::<Vec<String>>().join(", "))
            }
            Value::Null => "NULL",
        };
        write!(f, "{}", literal)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Str(inner_value) => {
                match other {
                    Self::Str(other_value) => inner_value == other_value,
                    _ => false,
                }
            },
            Self::Int(inner_value) => {
                match other {
                    Self::Int(other_value) => inner_value == other_value,
                    _ => false,
                }
            },
            Self::Expression(inner_value) => {
                match other {
                    Self::Expression(other_value) => {
                        inner_value.token_type() == other_value.token_type() &&
                        inner_value.literal() == other_value.literal()
                    },
                    _ => false,
                }
            },
            Self::Return(inner_value) => {
                match other {
                    Self::Return(other_value) => {
                        inner_value.deref() == other_value.deref() &&
                        inner_value.deref() == other_value.deref()
                    },
                    _ => false,
                }
            },
            Self::Function(left_expr, left_params, _) => {
                match other {
                    Self::Function(right_expr, right_params, _) => {
                        left_expr == right_expr &&
                        left_params == right_params
                    },
                    _ => false,
                }
            },
            Self::Bool(inner_value) => {
                match other {
                    Self::Bool(other_value) => inner_value == other_value,
                    _ => false,
                }
            },
            Self::Array(left_elements) => {
                match other {
                    Self::Array(right_elements) => {
                        left_elements.iter().zip(right_elements.iter()).all(|(l, r)| l == r)
                    },
                    _ => false,
                }
            }
            Self::Null => true,
        }
    }
}

impl Eq for Value {
    fn assert_receiver_is_total_eq(&self) {}
}

#[derive(Debug, Clone, Eq)]
pub enum Statement {
    Let(Identifier, Expr),
    Return(Expr),
    Expression(Expr),
    Block(Vec<Statement>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self {
            Statement::Let(id, expr) => format!("let {id} = {expr};"),
            Statement::Return(expr) => format!("return {expr};"),
            Statement::Expression(expr) => format!("{expr}"),
            Statement::Block(stmts) => {
                let mut output = String::new();
                for stmt in stmts {
                    output.push_str(&format!("{}", stmt));
                }
                output
            },
        };
        write!(f, "{}", literal)
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Statement::Let(id, expr) => {
                match other {
                    Statement::Let(other_id, other_expr) => {
                        id == other_id && 
                        expr.value() == other_expr.value() &&
                        expr.token_type() == other_expr.token_type()
                    },
                    _ => false,
                }
            },
            Statement::Return(expr) => {
                match other {
                    Statement::Return(other_expr) => {
                        expr.value() == other_expr.value() &&
                        expr.token_type() == other_expr.token_type()
                    },
                    _ => false,
                }
            },
            Statement::Expression(expr) => {
                match other {
                    Statement::Expression(other_expr) => {
                        expr.value() == other_expr.value() &&
                        expr.token_type() == other_expr.token_type()
                    },
                    _ => false,
                }
            },
            Statement::Block(stmts) => {
                match other {
                    Statement::Block(other_stmts) => {
                        stmts
                            .iter()
                            .zip(other_stmts.iter())
                            .all(|(stmt, other_stmt)| {
                                stmt == other_stmt
                            })
                    },
                    _ => false,
                }
            }
        }
    }
}

impl From<Statement> for String {
    fn from(value: Statement) -> Self {
        match value {
            Statement::Let(name, expr) => {
                format!("let {} = {};", name, expr)
            },
            Statement::Return(expr) => {
                format!("return {}", expr)
            },
            Statement::Expression(expr) => {
                format!("{}", expr)
            },
            Statement::Block(stmts) => {
                let mut output = String::new();
                for stmt in stmts {
                    output.push_str(&format!("{}", stmt));
                }
                output
            }
        }
    }
}

// EXPRESSIONS

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Identifier {
    pub(crate) fn new(value: &str) -> Identifier {
        Identifier { value: value.to_owned() }
    }
}

impl Expression for Identifier {
    fn value(&self) -> Value {
        Value::Str(self.value.clone())
    }
    fn token_type(&self) -> TokenType {
        TokenType::Ident
    }
    fn literal(&self) -> &str {
        &self.value
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.value.clone()
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier::new(value)
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Identifier::new(&value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Boolean {
    value: bool,
}

const TRUE_LITERAL: &str = "true";
const FALSE_LITERAL: &str = "false";

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Boolean {
    pub(crate) fn new(value: bool) -> Boolean {
        Boolean { value }
    }
}

impl Expression for Boolean {
    fn value(&self) -> Value {
        Value::Bool(self.value)
    }
    fn token_type(&self) -> TokenType {
        TokenType::Ident
    }
    fn literal(&self) -> &str {
        if self.value {
            TRUE_LITERAL
        } else {
            FALSE_LITERAL
        }
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Integer {
    pub(crate) value: i64,
    literal: String,
}

impl Integer {
    pub(crate) fn new(value: i64) -> Integer {
        let literal = format!("{value}");
        Integer { value, literal }
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Expression for Integer {
    fn value(&self) -> Value {
        Value::Int(self.value)
    }
    fn token_type(&self) -> TokenType {
        TokenType::Int
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct StringLiteral {
    value: String
}

impl StringLiteral {
    pub(crate) fn new(value: String) -> StringLiteral {
        StringLiteral { value }
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Expression for StringLiteral {
    fn value(&self) -> Value {
        Value::Str(self.value.clone())
    }
    fn token_type(&self) -> TokenType {
        TokenType::String
    }
    fn literal(&self) -> &str {
        &self.value
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct ArrayLiteral {
    pub(crate) elements: Vec<Expr>,
    literal: String,
}

impl ArrayLiteral {
    pub(crate) fn new(elements: Vec<Expr>) -> ArrayLiteral {
        let literal = format!("[{}]", elements.iter().map(|e| format!("{e}")).collect::<Vec<String>>().join(", "));
        ArrayLiteral { elements, literal }
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.elements.iter().map(|e| format!("{e}")).collect::<Vec<String>>().join(", "))
    }
}

impl Expression for ArrayLiteral {
    fn value(&self) -> Value {
        Value::Array(self.elements.clone())
    }
    fn token_type(&self) -> TokenType {
        TokenType::Lbracket
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct IndexExpression {
    left: Value,
    index: Value,
    literal: String,
}

impl IndexExpression {
    pub(crate) fn new(left: Expr, index: Expr) -> IndexExpression {
        let literal = format!("({left}[{index}])");
        IndexExpression {
            left: Value::Expression(left.clone()),
            index: Value::Expression(index.clone()),
            literal
        }
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

impl Expression for IndexExpression {
    fn value(&self) -> Value {
        Value::Int(1)
    }
    fn token_type(&self) -> TokenType {
        TokenType::Lbracket
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct PrefixedExpr {
    pub(crate) operator: TokenType,
    pub(crate) right: Value,
    literal: String,
}

impl PrefixedExpr {
    pub(crate) fn new(operator: TokenType, right: &Expr) -> PrefixedExpr {
        let literal = format!("{operator}{right}");
        PrefixedExpr { operator, right: Value::Expression(right.clone()), literal }
    }
}

impl Display for PrefixedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Expression for PrefixedExpr {
    fn value(&self) -> Value {
        self.right.clone()
    }
    fn token_type(&self) -> TokenType {
        self.operator.clone()
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct InfixExpr {
    pub(crate) operator: TokenType,
    pub(crate) left: Value,
    pub(crate) right: Value,
    literal: String,
}

impl InfixExpr {
    pub(crate) fn new(operator: TokenType, left: &Expr, right: &Expr) -> InfixExpr {
        let literal = format!("({left} {operator} {right})");
        InfixExpr {
            operator,
            left: Value::Expression(left.clone()),
            right: Value::Expression(right.clone()),
            literal,
        }
    }
}

impl Display for InfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl Expression for InfixExpr {
    fn value(&self) -> Value {
        Value::Expression(self.box_clone())
    }
    fn token_type(&self) -> TokenType {
        self.operator.clone()
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct IfExpr {
    pub(crate) condition: Value,
    pub(crate) consequence: Statement,
    pub(crate) alternative: Option<Statement>,
    literal: String,
}

impl IfExpr {
    pub(crate) fn new(condition: &Expr, consequence: Statement, alternative: Option<Statement>) -> IfExpr {
        let mut literal = format!("if {} {}", condition, consequence);
        if let Some(ref alternative_block) = alternative.as_ref() {
            literal.push_str(&format!("else {alternative_block}"));
        };
        IfExpr {
            condition: Value::Expression(condition.clone()),
            consequence,
            alternative,
            literal,
        }
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl Expression for IfExpr {
    fn value(&self) -> Value {
        Value::Expression(self.box_clone())
    }
    fn token_type(&self) -> TokenType {
        TokenType::If
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FunctionLiteral {
    pub(crate) parameters: Vec<Identifier>,
    pub(crate) body: Statement,
    literal: String,
}

impl FunctionLiteral {
    pub(crate) fn new(parameters: Vec<Identifier>, body: Statement) -> FunctionLiteral {
        let mut params: Vec<String> = vec![];
        for param in parameters.iter() {
            params.push(format!("{param}"));
        }
        let literal = format!(
            "fn ({:?}) {{ {} }}",
            params.join(", "),
            body
        );
        FunctionLiteral { parameters, body, literal }
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl Expression for FunctionLiteral {
    fn value(&self) -> Value {
        Value::Expression(self.box_clone())
    }
    fn token_type(&self) -> TokenType {
        TokenType::Function
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone, Eq)]
pub struct CallExpression {
    pub(crate) function: Statement,
    pub(crate) arguments: Vec<Expr>,
    literal: String,
}

impl PartialEq for CallExpression {
    fn eq(&self, other: &Self) -> bool {
        self.token_type() == other.token_type() &&
        self.literal == other.literal
    }
}

impl CallExpression {
    pub(crate) fn new(function: Statement, arguments: Vec<Expr>) -> CallExpression {
        let args_literal = arguments.iter().map(|a| a.literal().to_string()).collect::<Vec<String>>().join(", ");
        let literal = format!("{function}({args_literal})");
        CallExpression { function, arguments, literal }
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl Expression for CallExpression {
    fn value(&self) -> Value {
        Value::Expression(self.box_clone())
    }
    fn token_type(&self) -> TokenType {
        TokenType::Function
    }
    fn literal(&self) -> &str {
        &self.literal
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program::default()
    }
    pub(crate) fn add_stmt(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
    pub fn length(&self) -> usize {
        self.statements.len()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.statements.is_empty() {
            write!(f, "{}", self.statements.iter().map(|s| format!("{s}")).collect::<String>())
        } else {
            write!(f, "")
        }
    }
}

impl From<Program> for String {
    fn from(value: Program) -> Self {
        value
            .statements
            .into_iter()
            .map(|s| { let r: String = s.into(); r })
            .collect::<String>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_rendering() {
        let program = Program { statements: vec![
            Statement::Let(
                "my_var".into(),
                Box::new(Identifier::new("another_var"))
            ),
        ] };
        let result: String = program.into();
        let expected = "let my_var = another_var;".to_string();
        assert_eq!(result, expected);
    }
}
