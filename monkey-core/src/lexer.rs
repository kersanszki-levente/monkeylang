use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {

    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer { input: input.to_owned(), position: 0, read_position: 0, ch: None };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position == self.input.chars().count() {
            self.ch = None
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    fn skip_whitespace(&mut self) {
        while self.ch == Some(' ') || self.ch == Some('\t') || self.ch == Some('\n') || self.ch == Some('\r') {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> Option<&str> {
        let position = self.position;
        while self.ch.is_some_and(|ch| ch.is_alphabetic()) {
            self.read_char();
        };
        self.input.get(position..self.position)
    }

    fn read_number(&mut self) -> Option<&str> {
        let position = self.position;
        while self.ch.is_some_and(|ch| ch.is_numeric()) {
            self.read_char();
        };
        self.input.get(position..self.position)
    }

    fn read_string(&mut self) -> Option<String> {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == Some('"') || self.ch.is_none() {
                break
            }
        }
        self.input.get(position..self.position).map(|s| s.to_owned())
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let next = match self.ch {
            None => Token{ r#type: TokenType::EOF, literal: None },
            Some(next_char) => match next_char {
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        Token{ r#type: TokenType::Eq, literal: None }
                    } else {
                        Token{ r#type: TokenType::Assign, literal: None }
                    }
                },
                '+' => Token{ r#type: TokenType::Plus, literal: None },
                '-' => Token{ r#type: TokenType::Minus, literal: None },
                '!' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        Token{ r#type: TokenType::NotEq, literal: None }
                    } else {
                        Token{ r#type: TokenType::Bang, literal: None }
                    }
                },
                '/' => Token{ r#type: TokenType::Slash, literal: None },
                '*' => Token{ r#type: TokenType::Asterisk, literal: None },
                '<' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        Token{ r#type: TokenType::LtE, literal: None }
                    } else {
                        Token{ r#type: TokenType::Lt, literal: None }
                    }
                },
                '>' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        Token{ r#type: TokenType::GtE, literal: None }
                    } else {
                        Token{ r#type: TokenType::Gt, literal: None }
                    }
                },
                ';' => Token{ r#type: TokenType::Semicolon, literal: None },
                '(' => Token{ r#type: TokenType::Lparen, literal: None },
                ')' => Token{ r#type: TokenType::Rparen, literal: None },
                ',' => Token{ r#type: TokenType::Comma, literal: None },
                '{' => Token{ r#type: TokenType::Lbrace, literal: None },
                '}' => Token{ r#type: TokenType::Rbrace, literal: None },
                '[' => Token{ r#type: TokenType::Lbracket, literal: None },
                ']' => Token{ r#type: TokenType::Rbracket, literal: None },
                '"' => Token{ r#type: TokenType::String, literal: self.read_string() },
                _ => {
                    if next_char.is_alphabetic() {
                        let literal = self.read_identifier();
                        let token = match literal {
                            Some("let") => Token{ r#type: TokenType::Let, literal: None },
                            Some("fn") => Token{ r#type: TokenType::Function, literal: None },
                            Some("true") => Token{ r#type: TokenType::True, literal: None },
                            Some("false") => Token{ r#type: TokenType::False, literal: None },
                            Some("if") => Token{ r#type: TokenType::If, literal: None },
                            Some("else") => Token{ r#type: TokenType::Else, literal: None },
                            Some("return") => Token{ r#type: TokenType::Return, literal: None },
                            Some(_) => Token{ r#type: TokenType::Ident, literal: literal.map(|s| s.to_owned()) },
                            _ => unimplemented!(),
                        };
                        return token
                    } else if next_char.is_numeric() {
                        return Token{ r#type: TokenType::Int, literal: self.read_number().map(|s| s.to_owned()) }
                    } else {
                        Token{ r#type: TokenType::Illegal, literal: Some(next_char.to_string()) }
                    }
                },
            }
        };
        self.read_char();
        next
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token.r#type {
            TokenType::EOF => None,
            _ => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        struct TestCase {
            expected_type: TokenType,
            expected_literal: Option<String>,
        }

        let input = r#"let five = 5;
                       let ten = 10;

                       let add = fn(x, y) {
                         x + y;
                       };

                       let result = add(five, ten);
                       !-/*5;
                       5 < 10 > 5;

                       if (5 < 10) {
                         return true;
                       } else {
                         return false;
                       }

                       10 == 10;
                       10 != 9;
                       "foobar";
                       "foo bar";

                       10 <= 9;
                       10 >= 9;
                       [1, 2];
        "#;
        let test_cases: Vec<TestCase> = vec![
            TestCase { expected_type: TokenType::Let, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("five".to_string()) },
            TestCase { expected_type: TokenType::Assign, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("5".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Let, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("ten".to_string()) },
            TestCase { expected_type: TokenType::Assign, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Let, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("add".to_string()) },
            TestCase { expected_type: TokenType::Assign, expected_literal: None },
            TestCase { expected_type: TokenType::Function, expected_literal: None },
            TestCase { expected_type: TokenType::Lparen, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("x".to_string()) },
            TestCase { expected_type: TokenType::Comma, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("y".to_string()) },
            TestCase { expected_type: TokenType::Rparen, expected_literal: None },
            TestCase { expected_type: TokenType::Lbrace, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("x".to_string()) },
            TestCase { expected_type: TokenType::Plus, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("y".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Rbrace, expected_literal: None },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Let, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("result".to_string()) },
            TestCase { expected_type: TokenType::Assign, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("add".to_string()) },
            TestCase { expected_type: TokenType::Lparen, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("five".to_string()) },
            TestCase { expected_type: TokenType::Comma, expected_literal: None },
            TestCase { expected_type: TokenType::Ident, expected_literal: Some("ten".to_string()) },
            TestCase { expected_type: TokenType::Rparen, expected_literal: None },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Bang, expected_literal: None },
            TestCase { expected_type: TokenType::Minus, expected_literal: None },
            TestCase { expected_type: TokenType::Slash, expected_literal: None },
            TestCase { expected_type: TokenType::Asterisk, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("5".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("5".to_string()) },
            TestCase { expected_type: TokenType::Lt, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::Gt, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("5".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::If, expected_literal: None },
            TestCase { expected_type: TokenType::Lparen, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("5".to_string()) },
            TestCase { expected_type: TokenType::Lt, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::Rparen, expected_literal: None },
            TestCase { expected_type: TokenType::Lbrace, expected_literal: None },
            TestCase { expected_type: TokenType::Return, expected_literal: None },
            TestCase { expected_type: TokenType::True, expected_literal: None },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Rbrace, expected_literal: None },
            TestCase { expected_type: TokenType::Else, expected_literal: None },
            TestCase { expected_type: TokenType::Lbrace, expected_literal: None },
            TestCase { expected_type: TokenType::Return, expected_literal: None },
            TestCase { expected_type: TokenType::False, expected_literal: None },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Rbrace, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::Eq, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::NotEq, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("9".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::String, expected_literal: Some("foobar".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::String, expected_literal: Some("foo bar".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::LtE, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("9".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("10".to_string()) },
            TestCase { expected_type: TokenType::GtE, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("9".to_string()) },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::Lbracket, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("1".to_string()) },
            TestCase { expected_type: TokenType::Comma, expected_literal: None },
            TestCase { expected_type: TokenType::Int, expected_literal: Some("2".to_string()) },
            TestCase { expected_type: TokenType::Rbracket, expected_literal: None },
            TestCase { expected_type: TokenType::Semicolon, expected_literal: None },
            TestCase { expected_type: TokenType::EOF, expected_literal: None },
        ];

        let mut exer = Lexer::new(input);
        for case in test_cases {
            let token = exer.next_token();
            assert_eq!(token.r#type, case.expected_type);
            assert_eq!(token.literal, case.expected_literal);
        }
    }
}
