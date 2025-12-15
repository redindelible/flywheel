use flywheel_sources::Span;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub has_leading_whitespace: bool,
    pub span: Span,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum TokenType {
    Eof,
    Error,

    String,
    Integer,
    Hexadecimal,
    Binary,
    Float,
    Identifier,

    Fn,
    From,
    Struct,
    Import,
    Let,
    If,
    Else,
    While,
    Return,

    Period,
    Comma,
    Colon,
    Semicolon,
    Equal,
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
    pub(super) const fn keywords() -> &'static [(TokenType, &'static str)] {
        &[
            (TokenType::Fn, "fn"),
            (TokenType::From, "from"),
            (TokenType::Struct, "struct"),
            (TokenType::Import, "import"),
            (TokenType::Let, "let"),
            (TokenType::If, "if"),
            (TokenType::Else, "else"),
            (TokenType::While, "while"),
            (TokenType::Return, "return"),
        ]
    }

    pub const fn name(&self) -> &'static str {
        match self {
            TokenType::Eof => "the end of input",
            TokenType::Error => "an unexpected character",
            TokenType::String => "a string literal",
            TokenType::Integer => "a decimal number",
            TokenType::Hexadecimal => "a hexadecimal number",
            TokenType::Binary => "a binary number",
            TokenType::Float => "a floating-point number",
            TokenType::Identifier => "an identifier",
            TokenType::Fn => "'fn'",
            TokenType::From => "'from'",
            TokenType::Struct => "'struct'",
            TokenType::Import => "'import'",
            TokenType::Let => "'let'",
            TokenType::If => "'if'",
            TokenType::Else => "'else'",
            TokenType::While => "'while'",
            TokenType::Return => "'return'",
            TokenType::Period => "'.'",
            TokenType::Comma => "','",
            TokenType::Colon => "':'",
            TokenType::Semicolon => "';'",
            TokenType::Equal => "'='",
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
