use crate::frontend::InternedString;
use crate::frontend::source::{Location, SourceID};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub text: Option<InternedString>,
    pub has_leading_whitespace: bool,
    pub loc: Location,
}

impl Token {
    pub const fn new_eof(source: SourceID) -> Token {
        Token {
            ty: TokenType::EOF,
            text: None,
            has_leading_whitespace: true,
            loc: Location { source, offset: u32::MAX, length: 1 }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum TokenType {
    EOF,
    Error,

    Integer,
    Hexadecimal,
    Binary,
    Float,
    Identifier,

    Fn,
    Struct,
    If,
    Else,
    While,
    Return,

    Period,
    Comma,
    Colon,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    LeftAngle,
    RightAngle,
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    LeftArrow,
}

impl TokenType {
    pub const fn keywords() -> &'static [(TokenType, &'static str)] {
        &[
            (TokenType::Fn, "fn"),
            (TokenType::Struct, "struct"),
            (TokenType::If, "if"),
            (TokenType::Else, "else"),
            (TokenType::While, "while"),
            (TokenType::Return, "return"),
        ]
    }
    
    pub const fn name(&self) -> &'static str {
        match self {
            TokenType::EOF => "end of file",
            TokenType::Error => "an unexpected character",
            TokenType::Integer => "a decimal number",
            TokenType::Hexadecimal => "a hexadecimal number",
            TokenType::Binary => "a binary number",
            TokenType::Float => "a floating-point number",
            TokenType::Identifier => "an identifier",
            TokenType::Fn => "'fn'",
            TokenType::Struct => "'struct'",
            TokenType::If => "'if'",
            TokenType::Else => "'else'",
            TokenType::While => "'while'",
            TokenType::Return => "'return'",
            TokenType::Period => "'.'",
            TokenType::Comma => "','",
            TokenType::Colon => "':'",
            TokenType::Semicolon => "';'",
            TokenType::Plus => "'+'",
            TokenType::Minus => "'-'",
            TokenType::Star => "'*'",
            TokenType::Slash => "'/'",
            TokenType::Percent => "'%'",
            TokenType::LeftAngle => "'<'",
            TokenType::RightAngle => "'>'",
            TokenType::LeftParenthesis => "'('",
            TokenType::RightParenthesis => "')'",
            TokenType::LeftBracket => "'['",
            TokenType::RightBracket => "']'",
            TokenType::LeftBrace => "'{'",
            TokenType::RightBrace => "'}'",
            TokenType::LeftArrow => "'->'",
        }
    }
}