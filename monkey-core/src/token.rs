use core::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Illegal,
    EOF,

    Ident,
    Int,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Eq,
    NotEq,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self {
            TokenType::Illegal => "Illegal",
            TokenType::EOF => "",
            TokenType::Ident => unimplemented!(),
            TokenType::Int => unimplemented!(),
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Asterisk=> "*",
            TokenType::Slash => "/",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Lparen => "Illegal",
            TokenType::Rparen => "Illegal",
            TokenType::Lbrace => "Illegal",
            TokenType::Rbrace => "Illegal",
            TokenType::Function => "fn",
            TokenType::Let => "let",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
        };
        write!(f, "{}", literal)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) literal: Option<String>,
}

impl Token {
    pub fn is_type(&self, expected: TokenType) -> bool {
        self.r#type == expected
    }
}
