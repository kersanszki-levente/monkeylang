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
            None => Token::new(TokenType::EOF, None, self.position),
            Some(next_char) => match next_char {
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        Token::new(TokenType::Eq, None, self.position)
                    } else {
                        Token::new(TokenType::Assign, None, self.position)
                    }
                },
                '+' => Token::new(TokenType::Plus, None, self.position),
                '-' => Token::new(TokenType::Minus, None, self.position),
                '!' => {
                    if self.peek_char() == Some('=') {
                        let position = self.position;
                        self.read_char();
                        Token::new(TokenType::NotEq, None, position)
                    } else {
                        Token::new(TokenType::Bang, None, self.position)
                    }
                },
                '/' => Token::new(TokenType::Slash, None, self.position),
                '*' => Token::new(TokenType::Asterisk, None, self.position),
                '<' => {
                    if self.peek_char() == Some('=') {
                        let position = self.position;
                        self.read_char();
                        Token::new(TokenType::LtE, None, position)
                    } else {
                        Token::new(TokenType::Lt, None, self.position)
                    }
                },
                '>' => {
                    if self.peek_char() == Some('=') {
                        let position = self.position;
                        self.read_char();
                        Token::new(TokenType::GtE, None, position)
                    } else {
                        Token::new(TokenType::Gt, None, self.position)
                    }
                },
                ';' => Token::new(TokenType::Semicolon, None, self.position),
                '(' => Token::new(TokenType::Lparen, None, self.position),
                ')' => Token::new(TokenType::Rparen, None, self.position),
                ',' => Token::new(TokenType::Comma, None, self.position),
                '{' => Token::new(TokenType::Lbrace, None, self.position),
                '}' => Token::new(TokenType::Rbrace, None, self.position),
                '[' => Token::new(TokenType::Lbracket, None, self.position),
                ']' => Token::new(TokenType::Rbracket, None, self.position),
                '"' => {
                    let position = self.position;
                    Token::new(TokenType::String, self.read_string(), position)
                },
                _ => {
                    if next_char.is_alphabetic() {
                        let position = self.position;
                        let literal = self.read_identifier();
                        let token = match literal {
                            Some("let") => Token::new(TokenType::Let, None, position),
                            Some("fn") => Token::new(TokenType::Function, None, position),
                            Some("true") => Token::new(TokenType::True, None, position),
                            Some("false") => Token::new(TokenType::False, None, position),
                            Some("if") => Token::new(TokenType::If, None, position),
                            Some("else") => Token::new(TokenType::Else, None, position),
                            Some("return") => Token::new(TokenType::Return, None, position),
                            Some(_) => Token::new(TokenType::Ident, literal.map(|s| s.to_owned()), position),
                            _ => unimplemented!(),
                        };
                        return token
                    } else if next_char.is_numeric() {
                        let position = self.position;
                        return Token::new(TokenType::Int, self.read_number().map(|s| s.to_owned()), position)
                    } else {
                        let position = self.position;
                        Token::new(TokenType::Illegal, Some(next_char.to_string()), position)
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
            Token::new(TokenType::Let, None, 0),
            Token::new(TokenType::Ident, Some("five".to_string()), 4),
            Token::new(TokenType::Assign, None, 9),
            Token::new(TokenType::Int, Some("5".to_string()), 11),
            Token::new(TokenType::Semicolon, None, 12),
            Token::new(TokenType::Let, None, 37),
            Token::new(TokenType::Ident, Some("ten".to_string()), 41),
            Token::new(TokenType::Assign, None, 45),
            Token::new(TokenType::Int, Some("10".to_string()), 47),
            Token::new(TokenType::Semicolon, None, 49),
            Token::new(TokenType::Let, None, 75),
            Token::new(TokenType::Ident, Some("add".to_string()), 79),
            Token::new(TokenType::Assign, None, 83),
            Token::new(TokenType::Function, None, 85),
            Token::new(TokenType::Lparen, None, 87),
            Token::new(TokenType::Ident, Some("x".to_string()), 88),
            Token::new(TokenType::Comma, None, 89),
            Token::new(TokenType::Ident, Some("y".to_string()), 91),
            Token::new(TokenType::Rparen, None, 92),
            Token::new(TokenType::Lbrace, None, 94),
            Token::new(TokenType::Ident, Some("x".to_string()), 121),
            Token::new(TokenType::Plus, None, 123),
            Token::new(TokenType::Ident, Some("y".to_string()), 125),
            Token::new(TokenType::Semicolon, None, 126),
            Token::new(TokenType::Rbrace, None, 151),
            Token::new(TokenType::Semicolon, None, 152),
            Token::new(TokenType::Let, None, 178),
            Token::new(TokenType::Ident, Some("result".to_string()), 182),
            Token::new(TokenType::Assign, None, 189),
            Token::new(TokenType::Ident, Some("add".to_string()), 191),
            Token::new(TokenType::Lparen, None, 194),
            Token::new(TokenType::Ident, Some("five".to_string()), 195),
            Token::new(TokenType::Comma, None, 199),
            Token::new(TokenType::Ident, Some("ten".to_string()), 201),
            Token::new(TokenType::Rparen, None, 204),
            Token::new(TokenType::Semicolon, None, 205),
            Token::new(TokenType::Bang, None, 230),
            Token::new(TokenType::Minus, None, 231),
            Token::new(TokenType::Slash, None, 232),
            Token::new(TokenType::Asterisk, None, 233),
            Token::new(TokenType::Int, Some("5".to_string()), 234),
            Token::new(TokenType::Semicolon, None, 235),
            Token::new(TokenType::Int, Some("5".to_string()), 260),
            Token::new(TokenType::Lt, None, 262),
            Token::new(TokenType::Int, Some("10".to_string()), 264),
            Token::new(TokenType::Gt, None, 267),
            Token::new(TokenType::Int, Some("5".to_string()), 269),
            Token::new(TokenType::Semicolon, None, 270),
            Token::new(TokenType::If, None, 296),
            Token::new(TokenType::Lparen, None, 299),
            Token::new(TokenType::Int, Some("5".to_string()), 300),
            Token::new(TokenType::Lt, None, 302),
            Token::new(TokenType::Int, Some("10".to_string()), 304),
            Token::new(TokenType::Rparen, None, 306),
            Token::new(TokenType::Lbrace, None, 308),
            Token::new(TokenType::Return, None, 335),
            Token::new(TokenType::True, None, 342),
            Token::new(TokenType::Semicolon, None, 346),
            Token::new(TokenType::Rbrace, None, 371),
            Token::new(TokenType::Else, None, 373),
            Token::new(TokenType::Lbrace, None, 378),
            Token::new(TokenType::Return, None, 405),
            Token::new(TokenType::False, None, 412),
            Token::new(TokenType::Semicolon, None, 417),
            Token::new(TokenType::Rbrace, None, 442),
            Token::new(TokenType::Int, Some("10".to_string()), 468),
            Token::new(TokenType::Eq, None, 472),
            Token::new(TokenType::Int, Some("10".to_string()), 474),
            Token::new(TokenType::Semicolon, None, 476),
            Token::new(TokenType::Int, Some("10".to_string()), 501),
            Token::new(TokenType::NotEq, None, 504),
            Token::new(TokenType::Int, Some("9".to_string()), 507),
            Token::new(TokenType::Semicolon, None, 508),
            Token::new(TokenType::String, Some("foobar".to_string()), 533),
            Token::new(TokenType::Semicolon, None, 541),
            Token::new(TokenType::String, Some("foo bar".to_string()), 566),
            Token::new(TokenType::Semicolon, None, 575),
            Token::new(TokenType::Int, Some("10".to_string()), 601),
            Token::new(TokenType::LtE, None, 604),
            Token::new(TokenType::Int, Some("9".to_string()), 607),
            Token::new(TokenType::Semicolon, None, 608),
            Token::new(TokenType::Int, Some("10".to_string()), 633),
            Token::new(TokenType::GtE, None, 636),
            Token::new(TokenType::Int, Some("9".to_string()), 639),
            Token::new(TokenType::Semicolon, None, 640),
            Token::new(TokenType::Lbracket, None, 665),
            Token::new(TokenType::Int, Some("1".to_string()), 666),
            Token::new(TokenType::Comma, None, 667),
            Token::new(TokenType::Int, Some("2".to_string()), 669),
            Token::new(TokenType::Rbracket, None, 670),
            Token::new(TokenType::Semicolon, None, 671),
            Token::new(TokenType::EOF, None, 681),
        ];

        let mut lexer = Lexer::new(input);
        for expected_token in tokens{
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
