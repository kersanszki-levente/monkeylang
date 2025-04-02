use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    row_position: usize,
    col_position: usize,
    ch: Option<char>,
}

impl Lexer {

    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_owned(),
            position: 0,
            read_position: 0,
            ch: None,
            row_position: 1,
            col_position: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position == self.input.chars().count() {
            self.ch = None
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        if self.ch == Some('\n') || self.ch == Some('\r') {
            self.row_position += 1;
            self.col_position = 0;
        } else {
            self.col_position += 1;
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
            None => Token::new(TokenType::EOF, None, self.row_position, self.col_position),
            Some(next_char) => match next_char {
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        Token::new(TokenType::Eq, None, self.row_position, self.col_position)
                    } else {
                        Token::new(TokenType::Assign, None, self.row_position, self.col_position)
                    }
                },
                '+' => Token::new(TokenType::Plus, None, self.row_position, self.col_position),
                '-' => Token::new(TokenType::Minus, None, self.row_position, self.col_position),
                '!' => {
                    if self.peek_char() == Some('=') {
                        let row_position = self.row_position;
                        let col_position = self.col_position;
                        self.read_char();
                        Token::new(TokenType::NotEq, None, row_position, col_position)
                    } else {
                        Token::new(TokenType::Bang, None, self.row_position, self.col_position)
                    }
                },
                '/' => Token::new(TokenType::Slash, None, self.row_position, self.col_position),
                '*' => Token::new(TokenType::Asterisk, None, self.row_position, self.col_position),
                '<' => {
                    if self.peek_char() == Some('=') {
                        let row_position = self.row_position;
                        let col_position = self.col_position;
                        self.read_char();
                        Token::new(TokenType::LtE, None, row_position, col_position)
                    } else {
                        Token::new(TokenType::Lt, None, self.row_position, self.col_position)
                    }
                },
                '>' => {
                    if self.peek_char() == Some('=') {
                        let row_position = self.row_position;
                        let col_position = self.col_position;
                        self.read_char();
                        Token::new(TokenType::GtE, None, row_position, col_position)
                    } else {
                        Token::new(TokenType::Gt, None, self.row_position, self.col_position)
                    }
                },
                ';' => Token::new(TokenType::Semicolon, None, self.row_position, self.col_position),
                '(' => Token::new(TokenType::Lparen, None, self.row_position, self.col_position),
                ')' => Token::new(TokenType::Rparen, None, self.row_position, self.col_position),
                ',' => Token::new(TokenType::Comma, None, self.row_position, self.col_position),
                '{' => Token::new(TokenType::Lbrace, None, self.row_position, self.col_position),
                '}' => Token::new(TokenType::Rbrace, None, self.row_position, self.col_position),
                '[' => Token::new(TokenType::Lbracket, None, self.row_position, self.col_position),
                ']' => Token::new(TokenType::Rbracket, None, self.row_position, self.col_position),
                '"' => {
                    let row_position = self.row_position;
                    let col_position = self.col_position;
                    Token::new(TokenType::String, self.read_string(), row_position, col_position)
                },
                _ => {
                    if next_char.is_alphabetic() {
                        let row_position = self.row_position;
                        let col_position = self.col_position;
                        let literal = self.read_identifier();
                        let token = match literal {
                            Some("let") => Token::new(TokenType::Let, None, row_position, col_position),
                            Some("fn") => Token::new(TokenType::Function, None, row_position, col_position),
                            Some("true") => Token::new(TokenType::True, None, row_position, col_position),
                            Some("false") => Token::new(TokenType::False, None, row_position, col_position),
                            Some("if") => Token::new(TokenType::If, None, row_position, col_position),
                            Some("else") => Token::new(TokenType::Else, None, row_position, col_position),
                            Some("return") => Token::new(TokenType::Return, None, row_position, col_position),
                            Some(_) => Token::new(TokenType::Ident, literal.map(|s| s.to_owned()), row_position, col_position),
                            _ => unimplemented!(),
                        };
                        return token
                    } else if next_char.is_numeric() {
                        let row_position = self.row_position;
                        let col_position = self.col_position;
                        return Token::new(TokenType::Int, self.read_number().map(|s| s.to_owned()), row_position, col_position)
                    } else {
                        let row_position = self.row_position;
                        let col_position = self.col_position;
                        Token::new(TokenType::Illegal, Some(next_char.to_string()), row_position, col_position)
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
        let tokens : Vec<Token> = vec![
            Token::new(TokenType::Let, None, 1, 1),
            Token::new(TokenType::Ident, Some("five".to_string()), 1, 5),
            Token::new(TokenType::Assign, None, 1, 10),
            Token::new(TokenType::Int, Some("5".to_string()), 1, 12),
            Token::new(TokenType::Semicolon, None, 1, 13),
            Token::new(TokenType::Let, None, 2, 24),
            Token::new(TokenType::Ident, Some("ten".to_string()), 2, 28),
            Token::new(TokenType::Assign, None, 2, 32),
            Token::new(TokenType::Int, Some("10".to_string()), 2, 34),
            Token::new(TokenType::Semicolon, None, 2, 36),
            Token::new(TokenType::Let, None, 4, 24),
            Token::new(TokenType::Ident, Some("add".to_string()), 4, 28),
            Token::new(TokenType::Assign, None, 4, 32),
            Token::new(TokenType::Function, None, 4, 34),
            Token::new(TokenType::Lparen, None, 4, 36),
            Token::new(TokenType::Ident, Some("x".to_string()), 4, 37),
            Token::new(TokenType::Comma, None, 4, 38),
            Token::new(TokenType::Ident, Some("y".to_string()), 4, 40),
            Token::new(TokenType::Rparen, None, 4, 41),
            Token::new(TokenType::Lbrace, None, 4, 43),
            Token::new(TokenType::Ident, Some("x".to_string()), 5, 26),
            Token::new(TokenType::Plus, None, 5, 28),
            Token::new(TokenType::Ident, Some("y".to_string()), 5, 30),
            Token::new(TokenType::Semicolon, None, 5, 31),
            Token::new(TokenType::Rbrace, None, 6, 24),
            Token::new(TokenType::Semicolon, None, 6, 25),
            Token::new(TokenType::Let, None, 8, 24),
            Token::new(TokenType::Ident, Some("result".to_string()), 8, 28),
            Token::new(TokenType::Assign, None, 8, 35),
            Token::new(TokenType::Ident, Some("add".to_string()), 8, 37),
            Token::new(TokenType::Lparen, None, 8, 40),
            Token::new(TokenType::Ident, Some("five".to_string()), 8, 41),
            Token::new(TokenType::Comma, None, 8, 45),
            Token::new(TokenType::Ident, Some("ten".to_string()), 8, 47),
            Token::new(TokenType::Rparen, None, 8, 50),
            Token::new(TokenType::Semicolon, None, 8, 51),
            Token::new(TokenType::Bang, None, 9, 24),
            Token::new(TokenType::Minus, None, 9, 25),
            Token::new(TokenType::Slash, None, 9, 26),
            Token::new(TokenType::Asterisk, None, 9, 27),
            Token::new(TokenType::Int, Some("5".to_string()), 9, 28),
            Token::new(TokenType::Semicolon, None, 9, 29),
            Token::new(TokenType::Int, Some("5".to_string()), 10, 24),
            Token::new(TokenType::Lt, None, 10, 26),
            Token::new(TokenType::Int, Some("10".to_string()), 10, 28),
            Token::new(TokenType::Gt, None, 10, 31),
            Token::new(TokenType::Int, Some("5".to_string()), 10, 33),
            Token::new(TokenType::Semicolon, None, 10, 34),
            Token::new(TokenType::If, None, 12, 24),
            Token::new(TokenType::Lparen, None, 12, 27),
            Token::new(TokenType::Int, Some("5".to_string()), 12, 28),
            Token::new(TokenType::Lt, None, 12, 30),
            Token::new(TokenType::Int, Some("10".to_string()), 12, 32),
            Token::new(TokenType::Rparen, None, 12, 34),
            Token::new(TokenType::Lbrace, None, 12, 36),
            Token::new(TokenType::Return, None, 13, 26),
            Token::new(TokenType::True, None, 13, 33),
            Token::new(TokenType::Semicolon, None, 13, 37),
            Token::new(TokenType::Rbrace, None, 14, 24),
            Token::new(TokenType::Else, None, 14, 26),
            Token::new(TokenType::Lbrace, None, 14, 31),
            Token::new(TokenType::Return, None, 15, 26),
            Token::new(TokenType::False, None, 15, 33),
            Token::new(TokenType::Semicolon, None, 15, 38),
            Token::new(TokenType::Rbrace, None, 16, 24),
            Token::new(TokenType::Int, Some("10".to_string()), 18, 24),
            Token::new(TokenType::Eq, None, 18, 28),
            Token::new(TokenType::Int, Some("10".to_string()), 18, 30),
            Token::new(TokenType::Semicolon, None, 18, 32),
            Token::new(TokenType::Int, Some("10".to_string()), 19, 24),
            Token::new(TokenType::NotEq, None, 19, 27),
            Token::new(TokenType::Int, Some("9".to_string()), 19, 30),
            Token::new(TokenType::Semicolon, None, 19, 31),
            Token::new(TokenType::String, Some("foobar".to_string()), 20, 24),
            Token::new(TokenType::Semicolon, None, 20, 32),
            Token::new(TokenType::String, Some("foo bar".to_string()), 21, 24),
            Token::new(TokenType::Semicolon, None, 21, 33),
            Token::new(TokenType::Int, Some("10".to_string()), 23, 24),
            Token::new(TokenType::LtE, None, 23, 27),
            Token::new(TokenType::Int, Some("9".to_string()), 23, 30),
            Token::new(TokenType::Semicolon, None, 23, 31),
            Token::new(TokenType::Int, Some("10".to_string()), 24, 24),
            Token::new(TokenType::GtE, None, 24, 27),
            Token::new(TokenType::Int, Some("9".to_string()), 24, 30),
            Token::new(TokenType::Semicolon, None, 24, 31),
            Token::new(TokenType::Lbracket, None, 25, 24),
            Token::new(TokenType::Int, Some("1".to_string()), 25, 25),
            Token::new(TokenType::Comma, None, 25, 26),
            Token::new(TokenType::Int, Some("2".to_string()), 25, 28),
            Token::new(TokenType::Rbracket, None, 25, 29),
            Token::new(TokenType::Semicolon, None, 25, 30),
            Token::new(TokenType::EOF, None, 26, 9),
        ];

        let mut lexer = Lexer::new(input);
        for expected_token in tokens{
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
