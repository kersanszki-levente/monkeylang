use core::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Illegal,
    EOF,

    Ident,
    Int,
    String,

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
    Lbracket,
    Rbracket,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Eq,
    NotEq,
    LtE,
    GtE,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self {
            TokenType::Illegal => "Illegal",
            TokenType::EOF => "",
            TokenType::Ident => unimplemented!(),
            TokenType::Int => unimplemented!(),
            TokenType::String => unimplemented!(),
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
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Lbracket => "[",
            TokenType::Rbracket => "]",
            TokenType::Function => "fn",
            TokenType::Let => "let",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
            TokenType::LtE => "<=",
            TokenType::GtE => ">=",
        };
        write!(f, "{}", literal)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) literal: Option<String>,
    pub(crate) row_position: usize,
    pub(crate) col_position: usize,
}

impl Token {
    pub fn new(r#type: TokenType, literal: Option<String>, row_position: usize, col_position: usize) -> Token {
        Token { r#type, literal, row_position, col_position }
    }
    pub fn is_type(&self, expected: TokenType) -> bool {
        self.r#type == expected
    }
}
