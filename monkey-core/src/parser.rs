use core::fmt::Display;
use core::fmt::Formatter;
use core::result::Result;

use crate::ast::ArrayLiteral;
use crate::ast::Boolean;
use crate::ast::CallExpression;
use crate::ast::Expr;
use crate::ast::FunctionLiteral;
use crate::ast::Identifier;
use crate::ast::IfExpr;
use crate::ast::IndexExpression;
use crate::ast::InfixExpr;
use crate::ast::Integer;
use crate::ast::PrefixedExpr;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::StringLiteral;
use crate::error::ErrorMessage;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
enum OperatorPrecedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // fn(x)
    Index,       // array[index]
}

impl From<TokenType> for OperatorPrecedence {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Eq => OperatorPrecedence::Equals,
            TokenType::NotEq => OperatorPrecedence::Equals,
            TokenType::Lt => OperatorPrecedence::LessGreater,
            TokenType::Gt => OperatorPrecedence::LessGreater,
            TokenType::LtE => OperatorPrecedence::LessGreater,
            TokenType::GtE => OperatorPrecedence::LessGreater,
            TokenType::Plus => OperatorPrecedence::Sum,
            TokenType::Minus => OperatorPrecedence::Sum,
            TokenType::Slash => OperatorPrecedence::Product,
            TokenType::Asterisk => OperatorPrecedence::Product,
            TokenType::Lparen => OperatorPrecedence::Call,
            TokenType::Lbracket => OperatorPrecedence::Index,
            _ => OperatorPrecedence::Lowest,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError(ErrorMessage);

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

type ParserResult<T>=Result<T, ParserError>;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    next_token: Token,
    pub errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let next_token = lexer.next_token();
        Parser{lexer, cur_token, next_token, errors: vec![]}
    }

    fn next_token(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn cur_precedence(&self) -> OperatorPrecedence {
        self.cur_token.r#type.clone().into()
    }

    fn next_precedence(&self) -> OperatorPrecedence {
        self.next_token.r#type.clone().into()
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.cur_token.r#type == *t
    }

    fn next_token_is(&self, t: &TokenType) -> bool {
        self.next_token.r#type == *t
    }

    fn expect_next(&mut self, t: TokenType) -> ParserResult<()> {
        if self.next_token_is(&t) {
            self.next_token();
            Ok(())
        } else {
            let cause = format!("Expected {:?} after {:?}", t, self.cur_token.r#type);
            let error_message = ErrorMessage::new(
                &self.lexer.input,
                "stdin",
                self.cur_token.row_position,
                self.cur_token.col_position,
                &cause
            );
            Err(ParserError(error_message))
        }
    }

    fn parse_statement(&mut self) -> ParserResult<Statement> {
        match self.cur_token.r#type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> ParserResult<Statement> {
        self.expect_next(TokenType::Ident)?;
        let name = Identifier::new(&self.cur_token.literal.clone().unwrap(), self.cur_token.clone());
        self.expect_next(TokenType::Assign)?;
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::Lowest)?;
        if self.next_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        let statement = Statement::Let(name, expr);
        Ok(statement)
    }

    fn parse_return_statement(&mut self) -> ParserResult<Statement> {
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::Lowest)?;
        if self.next_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        let statement = Statement::Return(expr);
        Ok(statement)
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Statement> {

        let expression = self.parse_expression(OperatorPrecedence::Lowest)?;
        let statement = Statement::Expression(expression);

        if self.next_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        Ok(statement)
    }

    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> ParserResult<Expr> {
        let mut left_expr: Expr = match self.cur_token.r#type {
            TokenType::Ident => Box::new(self.parse_identifier()),
            TokenType::Int => Box::new(self.parse_integer_literal()?),
            TokenType::String => Box::new(self.parse_string_literal()?),
            TokenType::Minus | TokenType::Bang => self.parse_prefix_expression()?,
            TokenType::True => Box::new(Boolean::new(true, self.cur_token.clone())),
            TokenType::False => Box::new(Boolean::new(false, self.cur_token.clone())),
            TokenType::Lparen => self.parse_grouped_expression()?,
            TokenType::If => self.parse_if_expression()?,
            TokenType::Function => self.parse_function_literal()?,
            TokenType::Lbracket => Box::new(self.parse_array_literal()?),
            _ => {
                let cause = format!("{} is not a valid operator", self.cur_token.r#type);
                let error_message = ErrorMessage::new(
                    &self.lexer.input,
                    "stdin",
                    self.cur_token.row_position,
                    self.cur_token.col_position,
                    &cause
                );
                return Err(ParserError(error_message))
            }
        };
        while !self.next_token_is(&TokenType::Semicolon) && precedence < self.next_precedence() {
            let next_token_type = self.next_token.r#type.clone();
            left_expr = match next_token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt
                | TokenType::LtE
                | TokenType::GtE => {
                    self.next_token();
                    self.parse_infix_expression(left_expr)?
                },
                TokenType::Lparen => {
                    self.next_token();
                    self.parse_call_expression(left_expr)?
                },
                TokenType::Lbracket => {
                    self.next_token();
                    self.parse_index_expression(left_expr)?
                },
                _ => left_expr,
            };
        }
        Ok(left_expr)
    }

    fn parse_identifier(&self) -> Identifier {
        Identifier::new(self.cur_token.literal.as_ref().unwrap(), self.cur_token.clone())
    }

    fn parse_integer_literal(&self) -> ParserResult<Integer> {
        let value: i64 = match &self.cur_token.literal {
            Some(literal) => {
                literal
                .parse()
                .map_err(|_| {
                        let cause = format!("Failed to parse {} as integer", &literal);
                        let error_message = ErrorMessage::new(
                            &self.lexer.input,
                            "stdin",
                            self.cur_token.row_position,
                            self.cur_token.col_position,
                            &cause
                        );
                        ParserError(error_message)
                    })?
            },
            None => {
                let cause = "Failed to parse integer".to_string();
                let error_message = ErrorMessage::new(
                    &self.lexer.input,
                    "stdin",
                    self.cur_token.row_position,
                    self.cur_token.col_position,
                    &cause
                );
                return Err(ParserError(error_message))
            },
        };
        Ok(Integer::new(value, self.cur_token.clone()))
    }

    fn parse_string_literal(&self) -> ParserResult<StringLiteral> {
        if let Some(literal) = &self.cur_token.literal {
            Ok(StringLiteral::new(literal.to_string(), self.cur_token.clone()))
        } else {
            let cause = "Failed to parse string".to_string();
            let error_message = ErrorMessage::new(
                &self.lexer.input,
                "stdin",
                self.cur_token.row_position,
                self.cur_token.col_position,
                &cause
            );
            Err(ParserError(error_message))
        }
    }

    fn parse_array_literal(&mut self) -> ParserResult<ArrayLiteral> {
        Ok(ArrayLiteral::new(self.parse_expression_list(TokenType::Rbracket)?, self.cur_token.clone()))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> ParserResult<Vec<Expr>> {
        let mut results = vec![];

        if self.next_token_is(&end) {
            self.next_token();
            return Ok(results)
        }

        self.next_token();
        results.push(self.parse_expression(OperatorPrecedence::Lowest)?);

        loop {
            if !self.next_token_is(&TokenType::Comma) {
                break
            }
            self.next_token();
            self.next_token();
            results.push(self.parse_expression(OperatorPrecedence::Lowest)?);
        }

        self.expect_next(end)?;
        Ok(results)
    }

    fn parse_grouped_expression(&mut self) -> ParserResult<Expr> {
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::Lowest)?;
        self.expect_next(TokenType::Rparen)?;
        Ok(expr)
    }

    fn parse_prefix_expression(&mut self) -> ParserResult<Expr> {
        let operator = self.cur_token.r#type.clone();
        self.next_token();
        let right = self.parse_expression(OperatorPrecedence::Prefix)?;
        Ok(Box::new(PrefixedExpr::new(operator, &right, self.cur_token.clone())))
    }

    fn parse_infix_expression(&mut self, left: Expr) -> ParserResult<Expr> {
        let operator = self.cur_token.r#type.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Box::new(InfixExpr::new(operator, &left, &right, self.cur_token.clone())))
    }

    fn parse_if_expression(&mut self) -> ParserResult<Expr> {
        self.expect_next(TokenType::Lparen)?;
        self.next_token();
        let condition = self
            .parse_expression(OperatorPrecedence::Lowest)?;

        self.expect_next(TokenType::Rparen)?;
        self.expect_next(TokenType::Lbrace)?;
        let consequence = self.parse_block_statement()?;

        let alternative = if self.next_token_is(&TokenType::Else) {
            self.next_token();
            self.expect_next(TokenType::Lbrace)?;
            Some(self.parse_block_statement()?)
        } else { None };

        let expression: Expr = Box::new(IfExpr::new(
            &condition,
            consequence,
            alternative,
            self.cur_token.clone()
        ));
        Ok(expression)
    }

    fn parse_block_statement(&mut self) -> ParserResult<Statement> {
        let mut statements = vec![];
        self.next_token();

        while !self.cur_token_is(&TokenType::Rbrace) && !self.cur_token_is(&TokenType::EOF) {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Statement::Block(statements))
    }

    fn parse_function_literal(&mut self) -> ParserResult<Expr> {
        self.expect_next(TokenType::Lparen)?;
        let parameters = self.parse_function_parameters()?;

        self.expect_next(TokenType::Lbrace)?;
        let body = self.parse_block_statement()?;

        let func = FunctionLiteral::new(parameters, body, self.cur_token.clone());
        Ok(Box::new(func))
    }

    fn parse_function_parameters(&mut self) -> ParserResult<Vec<Identifier>> {
        let mut parameters = vec![];

        if self.next_token_is(&TokenType::Rparen) {
            self.next_token();
            return Ok(parameters)
        }

        self.next_token();

        match &self.cur_token.literal {
            Some(literal) => parameters.push(Identifier::new(literal, self.cur_token.clone())),
            None => {
                let cause = format!("Expected function parameter, got {:?}", self.cur_token);
                let error_message = ErrorMessage::new(
                    &self.lexer.input,
                    "stdin",
                    self.cur_token.row_position,
                    self.cur_token.col_position,
                    &cause
                );
                return Err(ParserError(error_message))
            }
        };

        while self.next_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            match &self.cur_token.literal {
                Some(literal) => parameters.push(Identifier::new(literal, self.cur_token.clone())),
                None => {
                    let cause = format!("Expected function parameter, got {:?}", self.cur_token);
                    let error_message = ErrorMessage::new(
                        &self.lexer.input,
                        "stdin",
                        self.cur_token.row_position,
                        self.cur_token.col_position,
                        &cause
                    );
                    return Err(ParserError(error_message))
                }
            };
        }

        self.expect_next(TokenType::Rparen)?;

        Ok(parameters)
    }

    fn parse_call_expression(&mut self, function: Expr) -> ParserResult<Expr> {
        let arguments = self.parse_call_arguments()?;
        Ok(Box::new(CallExpression::new(Statement::Expression(function), arguments, self.cur_token.clone())))
    }

    fn parse_index_expression(&mut self, left: Expr) -> ParserResult<Expr> {
        self.next_token();
        let index = self.parse_expression(OperatorPrecedence::Lowest)?;
        self.expect_next(TokenType::Rbracket)?;
        Ok(Box::new(IndexExpression::new(left, index, self.cur_token.clone())))
    }

    fn parse_call_arguments(&mut self) -> ParserResult<Vec<Expr>> {
        let mut args = vec![];
        if self.next_token_is(&TokenType::Rparen) {
            self.next_token();
            return Ok(args)
        }

        self.next_token();
        args.push(self.parse_expression(OperatorPrecedence::Lowest)?);

        while self.next_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(OperatorPrecedence::Lowest)?);
        }

        self.expect_next(TokenType::Rparen)?;

        Ok(args)
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        
        while !self.cur_token_is(&TokenType::EOF) {
            match self.parse_statement() {
                Ok(stmt) => program.add_stmt(stmt),
                Err(e) => self.errors.push(e),
            }
            self.next_token();
        }

        program
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::CallExpression;

    use super::*;

    #[test]
    fn test_let_statements() {
        // valid statements
        let input = r"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.length(), 3);

        let test_cases = vec![
            Statement::Let(
                Identifier::new("x", Token::new(TokenType::Ident, Some("x".to_string()), 2, 13)),
                Box::new(Integer::new(5, Token::new(TokenType::Int, Some("5".to_string()), 2, 17)))
            ),
            Statement::Let(
                Identifier::new("y", Token::new(TokenType::Ident, Some("y".to_string()), 3, 13)),
                Box::new(Integer::new(10, Token::new(TokenType::Int, Some("10".to_string()), 3, 17)))
            ),
            Statement::Let(
                Identifier::new("foobar", Token::new(TokenType::Ident, Some("foobar".to_string()), 4, 13)),
                Box::new(Integer::new(838383, Token::new(TokenType::Int, Some("838383".to_string()), 4, 22)))
            ),
        ];
        for (stmt, expected) in program.statements.into_iter().zip(test_cases) {
            assert_eq!(stmt, expected)
        }

        // invalid statements
        let test_cases = vec![
            "let x 5;",
            "let = 10;",
            "let 838383;"
        ];

        for case in test_cases {
            let l = Lexer::new(case);
            let mut p = Parser::new(l);
            p.parse();
            if p.errors.is_empty() {
                panic!("Test case should have returned Err for `{case}`")
            };
        }
    }

    #[test]
    fn test_return_statements() {
        // valid statements
        let input = r"
        return 5;
        return y;
        return 838383;
        ";
        // return add(5);

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 3);

        let test_cases = vec![
            Statement::Return(Box::new(Integer::new(5, Token::new(TokenType::Int, Some("".to_string()), 2, 16)))),
            Statement::Return(Box::new(Identifier::new("y", Token::new(TokenType::Ident, Some("y".to_string()), 3, 16)))),
            Statement::Return(Box::new(Integer::new(838383, Token::new(TokenType::Int, Some("838383".to_string()), 4, 16)))),
        ];

        assert_eq!(test_cases.len(), program.statements.len());
        for (parsed_node, expected_node) in test_cases.into_iter().zip(program.statements) {
            assert_eq!(parsed_node, expected_node);
        };
    }

    #[test]
    fn test_identifier_expression() {
        // valid statements
        let input = "foobar;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert_eq!(program.statements.len(), 1);

        let expected = crate::ast::Program { statements: vec![
            crate::ast::Statement::Expression(Box::new(crate::ast::Identifier::new(
                "foobar", Token::new(TokenType::Ident, Some("foobar".to_string()), 1, 1)))),
        ] };
        assert_eq!(program, expected);
    }

    #[test]
    fn test_integer_literal_expression() {
        // valid statements
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert_eq!(program.statements.len(), 1);

        let expected = crate::ast::Program { statements: vec![
            crate::ast::Statement::Expression(Box::new(crate::ast::Integer::new(5, Token::new(TokenType::Int, Some("5".to_string()), 1, 1)))),
        ] };
        assert_eq!(program, expected);
    }

    #[test]
    fn test_boolean_expression() {
        // valid statements
        let input = "true;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert_eq!(program.statements.len(), 1);

        let expected = crate::ast::Program { statements: vec![
            crate::ast::Statement::Expression(Box::new(crate::ast::Boolean::new(
                true, Token::new(TokenType::True, Some("true".to_string()), 1, 1)))),
        ] };
        assert_eq!(program, expected);
    }

    #[test]
    fn test_string_expression() {
        // valid statements
        let input = "\"hello world\"";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert_eq!(program.statements.len(), 1);

        let expected = crate::ast::Program { statements: vec![
            crate::ast::Statement::Expression(Box::new(crate::ast::StringLiteral::new(
                "hello world".to_string(), Token::new(TokenType::String, Some("\"hello world\"".to_string()), 1, 1)))),
        ] };
        assert_eq!(program, expected);
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let case_a: Expr = Box::new(Integer::new(5, Token::new(TokenType::Int, Some("5".to_string()), 1, 2)));
        let case_b: Expr = Box::new(Integer::new(15, Token::new(TokenType::Int, Some("15".to_string()), 1, 2)));
        let test_cases = vec![
            ("!5", PrefixedExpr::new(TokenType::Bang, &case_a, Token::new(TokenType::Bang, Some("!".to_string()), 1, 1))),
            ("-15", PrefixedExpr::new(TokenType::Minus, &case_b, Token::new(TokenType::Minus, Some("-".to_string()), 1, 1))),
        ];

        for (input, expected_expression) in test_cases {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse();

            assert_eq!(program.statements.len(), 1);

            let expected = crate::ast::Program { statements: vec![
                Statement::Expression(Box::new(expected_expression))
            ] };

            assert_eq!(program, expected);
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let left: Expr = Box::new(Integer::new(5, Token::new(TokenType::Int, Some("5".to_string()), 1, 1)));
        let right: Expr = Box::new(Integer::new(5, Token::new(TokenType::Int, Some("5".to_string()), 1, 5)));
        let test_cases = vec![
            ("5 + 5", InfixExpr::new(TokenType::Plus, &left, &right, Token::new(TokenType::Plus, Some("+".to_string()), 1, 3))),
            ("5 - 5", InfixExpr::new(TokenType::Minus, &left, &right, Token::new(TokenType::Minus, Some("-".to_string()), 1, 3))),
            ("5 * 5", InfixExpr::new(TokenType::Asterisk, &left, &right, Token::new(TokenType::Asterisk, Some("*".to_string()), 1, 3))),
            ("5 / 5", InfixExpr::new(TokenType::Slash, &left, &right, Token::new(TokenType::Slash, Some("/".to_string()), 1, 3))),
            ("5 > 5", InfixExpr::new(TokenType::Gt, &left, &right, Token::new(TokenType::Gt, Some(">".to_string()), 1, 3))),
            ("5 < 5", InfixExpr::new(TokenType::Lt, &left, &right, Token::new(TokenType::Lt, Some("<".to_string()), 1, 3))),
            ("5 == 5", InfixExpr::new(TokenType::Eq, &left, &right, Token::new(TokenType::Eq, Some("==".to_string()), 1, 3))),
            ("5 != 5", InfixExpr::new(TokenType::NotEq, &left, &right, Token::new(TokenType::NotEq, Some("!=".to_string()), 1, 3))),
            ("5 <= 5", InfixExpr::new(TokenType::LtE, &left, &right, Token::new(TokenType::LtE, Some("<=".to_string()), 1, 3))),
            ("5 >= 5", InfixExpr::new(TokenType::GtE, &left, &right, Token::new(TokenType::GtE, Some(">=".to_string()), 1, 3))),
        ];

        for (input, expected_expression) in test_cases {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse();

            assert_eq!(program.statements.len(), 1);

            let expected = crate::ast::Program { statements: vec![
                Statement::Expression(Box::new(expected_expression))
            ] };

            assert_eq!(program, expected);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let test_cases = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
        ];

        for (input, expected) in test_cases {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse();
            assert_eq!(format!("{program}"), expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let test_case = "if (x < y) { x }";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let x: Expr = Box::new(Identifier::new("x", Token::new(TokenType::Ident, Some("x".to_string()), 1, 5)));
        let y: Expr = Box::new(Identifier::new("y", Token::new(TokenType::Ident, Some("y".to_string()), 1, 9)));
        let condition: Expr = Box::new(InfixExpr::new(
            TokenType::Lt, &x, &y,
            Token::new(TokenType::Lt, Some("<".to_string()), 1, 7)
        ));
        let expression: Expr = Box::new(IfExpr::new(
            &condition,
            Statement::Block(vec![Statement::Expression(x)]),
            None,
            Token::new(TokenType::If, Some("if".to_string()), 1, 1)
        ));
        let expected = Program { statements: vec![
            Statement::Expression(expression)
        ] };

        assert_eq!(program, expected);
    }

    #[test]
    fn test_if_else_expression() {
        let test_case = "if (x < y) { x } else { y }";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let x: Expr = Box::new(Identifier::new("x", Token::new(TokenType::Ident, Some("x".to_string()), 1, 5)));
        let y: Expr = Box::new(Identifier::new("y", Token::new(TokenType::Ident, Some("y".to_string()), 1, 9)));
        let condition: Expr = Box::new(InfixExpr::new(
            TokenType::Lt, &x, &y,
            Token::new(TokenType::Lt, Some("<".to_string()), 1, 7)
        ));
        let expression: Expr = Box::new(IfExpr::new(
            &condition,
            Statement::Block(vec![Statement::Expression(x)]),
            Some(Statement::Block(vec![Statement::Expression(y)])),
            Token::new(TokenType::If, Some("if".to_string()), 1, 1)
        ));
        let expected = crate::ast::Program { statements: vec![
            Statement::Expression(expression)
        ] };

        assert_eq!(program, expected);
    }

    #[test]
    fn test_function_literal() {
        let test_case = "fn(x, y) { return x + y; }";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let x = Identifier::new("x", Token::new(TokenType::Ident, Some("x".to_string()), 1, 4));
        let x_expr: Expr = Box::new(x.clone());
        let y = Identifier::new("y", Token::new(TokenType::Ident, Some("y".to_string()), 1, 7));
        let y_expr: Expr = Box::new(y.clone());
        let returned_expression: Expr = Box::new(InfixExpr::new(
            TokenType::Plus, &x_expr, &y_expr,
            Token::new(TokenType::Plus, Some("+".to_string()), 1, 20)
        ));
        let expression: Expr = Box::new(FunctionLiteral::new(
            vec![x, y], Statement::Block(vec![
                Statement::Return(returned_expression)
            ]),
            Token::new(TokenType::Function, Some("fn".to_string()), 1, 1)
        ));
        let expected = Program { statements: vec![
            Statement::Expression(expression)
        ]};

        assert_eq!(program, expected);
    }

    #[test]
    fn test_call_expression_parsing() {
        let test_case = "add(1, 2 * 3, 4 + 5)";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let function: Expr = Box::new(Identifier::new("add", Token::new(TokenType::Ident, Some("add".to_string()), 1, 1)));
        let first: Expr = Box::new(Integer::new(1, Token::new(TokenType::Int, Some("1".to_string()), 1, 5)));
        let second_left: Expr = Box::new(Integer::new(2, Token::new(TokenType::Int, Some("2".to_string()), 1, 7)));
        let second_right: Expr = Box::new(Integer::new(3, Token::new(TokenType::Int, Some("3".to_string()), 1, 11)));
        let second: Expr = Box::new(InfixExpr::new(
            TokenType::Asterisk,
            &second_left,
            &second_right,
            Token::new(TokenType::Asterisk, Some("*".to_string()), 1, 9)
        ));
        let third_left: Expr = Box::new(Integer::new(4, Token::new(TokenType::Int, Some("4".to_string()), 1, 14)));
        let third_right: Expr = Box::new(Integer::new(5, Token::new(TokenType::Int, Some("5".to_string()), 1, 18)));
        let third: Expr = Box::new(InfixExpr::new(
            TokenType::Plus,
            &third_left,
            &third_right,
            Token::new(TokenType::Plus, Some("+".to_string()), 1, 16)
        ));
        let arguments: Vec<Expr> = vec![ first, second, third ];
        let expression: Expr = Box::new(CallExpression::new(
            Statement::Expression(function),
            arguments,
            Token::new(TokenType::Lparen, Some("(".to_string()), 1, 4)
        ));
        let expectation = Program { statements: vec![
            Statement::Expression(expression)
        ] };

        assert_eq!(program, expectation);
    }

    #[test]
    fn test_parse_array_literal() {
        let test_case = "[1, 2 * 2, 3 + 3]";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 1);

        let first: Expr = Box::new(Integer::new(1, Token::new(TokenType::Int, Some("1".to_string()), 1, 2)));
        let first_infix_left_item: Expr = Box::new(Integer::new(2, Token::new(TokenType::Int, Some("2".to_string()), 1, 5)));
        let first_infix_right_item: Expr = Box::new(Integer::new(2, Token::new(TokenType::Int, Some("2".to_string()), 1, 9)));
        let second: Expr = Box::new(InfixExpr::new(
            TokenType::Asterisk,
            &first_infix_left_item,
            &first_infix_right_item,
            Token::new(TokenType::Asterisk, Some("*".to_string()), 1, 7)
        ));
        let second_infix_left_item: Expr = Box::new(Integer::new(3, Token::new(TokenType::Int, Some("3".to_string()), 1, 12)));
        let second_infix_right_item: Expr = Box::new(Integer::new(3, Token::new(TokenType::Int, Some("3".to_string()), 1, 16)));
        let third: Expr = Box::new(InfixExpr::new(
            TokenType::Plus,
            &second_infix_left_item,
            &second_infix_right_item,
            Token::new(TokenType::Plus, Some("+".to_string()), 1, 16)
        ));
        let arr = ArrayLiteral::new(
            vec![first, second, third],
            Token::new(TokenType::Lbrace, Some("[".to_string()), 1, 1)
        );
        let expectation = Program { statements: vec![
            Statement::Expression(Box::new(arr))
        ]};

        assert_eq!(program, expectation);

        let test_case = "[1, ";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let _ = p.parse();

        assert!(!p.errors.is_empty());
    }

    #[test]
    fn test_parse_index_expression() {
        let test_case = "myArray[1 + 1]";
        let l = Lexer::new(test_case);
        let mut p = Parser::new(l);
        let program = p.parse();

        assert!(p.errors.is_empty());
        assert_eq!(program.statements.len(), 1);
    }
}
