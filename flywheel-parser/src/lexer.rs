use std::ops::Range;
use flywheel_sources::{Source, Span};
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct LexerError {
    span: Span,
}

#[derive(logos::Logos)]
enum LogosToken {
    #[regex(r"[ \t\r\n]+")]
    Whitespace,

    #[regex(r"#[^\n]+")]
    Comment,

    #[token("fn")]
    Fn,

    #[token("from")]
    From,

    #[token("struct")]
    Struct,

    #[token("import")]
    Import,

    #[token("let")]
    Let,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    #[token("return")]
    Return,

    #[regex(r"[_\p{ID_Start}][_\p{ID_Continue}]*")]
    Identifier,

    #[regex(r#""([^"\\]|\\.)*""#)]
    String,

    #[regex(r"0b[0-1]+")]
    Binary,

    #[regex(r"0x[a-fA-F0-9]+")]
    Hexadecimal,

    #[regex(r"[0-9]+")]
    Integer,

    #[regex(r"[0-9]+\.[0-9]+")]
    Float,

    #[token("->")]
    LeftArrow,

    #[token(".")]
    Period,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token("=")]
    Equal,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("<")]
    LeftAngle,

    #[token(">")]
    RightAngle,
}

fn advance_logos(lexer: &mut Option<logos::Lexer<'_, LogosToken>>) -> Option<(TokenType, bool, Range<usize>)> {
    use LogosToken::*;

    if let Some(inner) = lexer.as_mut() {
        let mut has_leading_whitespace = false;
        while let Some(logos_type) = inner.next() {
            let token_type = match logos_type {
                Ok(Whitespace | Comment) => {
                    has_leading_whitespace = true;
                    continue;
                },
                Err(()) => TokenType::Error,
                Ok(Fn) => TokenType::Fn,
                Ok(From) => TokenType::From,
                Ok(Struct) => TokenType::Struct,
                Ok(Import) => TokenType::Import,
                Ok(Let) => TokenType::Let,
                Ok(If) => TokenType::If,
                Ok(Else) => TokenType::Else,
                Ok(While) => TokenType::While,
                Ok(Return) => TokenType::Return,
                Ok(Identifier) => TokenType::Identifier,
                Ok(String) => TokenType::String,
                Ok(Binary) => TokenType::Binary,
                Ok(Hexadecimal) => TokenType::Hexadecimal,
                Ok(Integer) => TokenType::Integer,
                Ok(Float) => TokenType::Float,
                Ok(LeftArrow) => TokenType::LeftArrow,
                Ok(Period) => TokenType::Period,
                Ok(Comma) => TokenType::Comma,
                Ok(Colon) => TokenType::Colon,
                Ok(Semicolon) => TokenType::Semicolon,
                Ok(Equal) => TokenType::Equal,
                Ok(Plus) => TokenType::Plus,
                Ok(Minus) => TokenType::Minus,
                Ok(Star) => TokenType::Star,
                Ok(Slash) => TokenType::Slash,
                Ok(Percent) => TokenType::Percent,
                Ok(LeftBrace) => TokenType::LeftBrace,
                Ok(RightBrace) => TokenType::RightBrace,
                Ok(LeftParenthesis) => TokenType::LeftParenthesis,
                Ok(RightParenthesis) => TokenType::RightParenthesis,
                Ok(LeftBracket) => TokenType::LeftBracket,
                Ok(RightBracket) => TokenType::RightBracket,
                Ok(LeftAngle) => TokenType::LeftAngle,
                Ok(RightAngle) => TokenType::RightAngle,
            };

            return Some((token_type, has_leading_whitespace, inner.span()));
        }

        *lexer = None;
    }

    None
}

pub(super) struct Lexer<'a> {
    source: &'a Source,
    eof: Span,
    inner: Option<logos::Lexer<'a, LogosToken>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Source) -> Lexer<'a> {
        let text = source.text();
        let eof = source.add_span(text.len()..text.len()+1);
        Lexer {
            source,
            eof,
            inner: Some(logos::Lexer::new(text)),
        }
    }

    pub fn advance(&mut self) -> Token {
        let token = match advance_logos(&mut self.inner) {
            Some((ty, has_leading_whitespace, span)) => Token {
                ty,
                has_leading_whitespace,
                span: self.source.add_span(span),
            },
            None => self.eof(),
        };
        token
    }

    pub fn eof(&self) -> Token {
        Token {
            ty: TokenType::Eof,
            has_leading_whitespace: false,
            span: self.eof,
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.inner.as_ref().map_or(self.source.text().len()..self.source.text().len(), |lexer| lexer.span())
    }

    pub fn source(&self) -> &'a Source {
        self.source
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use flywheel_sources::SourceMap;
    use crate::token::TokenType;
    use camino::Utf8PathBuf;

    fn test_lexer(input: &str, expected: &[(TokenType, &str)]) {
        let sources = SourceMap::new();
        let source = sources.add_file("test.fly", input.to_string());
        let mut lexer = Lexer::new(source);

        for (expected_ty, expected_text) in expected {
            let token = lexer.advance();
            assert_eq!(token.ty, *expected_ty, "Token type mismatch for '{}'", expected_text);
            assert_eq!(sources.get_span(token.span), *expected_text, "Token text mismatch");
        }

        let eof_token = lexer.advance();
        assert_eq!(eof_token.ty, TokenType::Eof);
    }

    #[test]
    fn test_keywords() {
        test_lexer(
            "fn from struct import let if else while return",
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
            ],
        );
    }

    #[test]
    fn test_identifiers() {
        test_lexer(
            "foo _bar baz123",
            &[
                (TokenType::Identifier, "foo"),
                (TokenType::Identifier, "_bar"),
                (TokenType::Identifier, "baz123"),
            ],
        );
    }

    #[test]
    fn test_numeric_literals() {
        test_lexer(
            "123 0b1010 0x123abc 123.456",
            &[
                (TokenType::Integer, "123"),
                (TokenType::Binary, "0b1010"),
                (TokenType::Hexadecimal, "0x123abc"),
                (TokenType::Float, "123.456"),
            ],
        );
    }

    #[test]
    fn test_string_literals() {
        test_lexer(
            r#""hello" "with \" escape""#,
            &[
                (TokenType::String, r#""hello""#),
                (TokenType::String, r#""with \" escape""#),
            ],
        );
    }

    #[test]
    fn test_punctuation() {
        test_lexer(
            "-> . , : ; = + - * / % { } ( ) [ ] < >",
            &[
                (TokenType::LeftArrow, "->"),
                (TokenType::Period, "."),
                (TokenType::Comma, ","),
                (TokenType::Colon, ":"),
                (TokenType::Semicolon, ";"),
                (TokenType::Equal, "="),
                (TokenType::Plus, "+"),
                (TokenType::Minus, "-"),
                (TokenType::Star, "*"),
                (TokenType::Slash, "/"),
                (TokenType::Percent, "%"),
                (TokenType::LeftBrace, "{"),
                (TokenType::RightBrace, "}"),
                (TokenType::LeftParenthesis, "("),
                (TokenType::RightParenthesis, ")"),
                (TokenType::LeftBracket, "["),
                (TokenType::RightBracket, "]"),
                (TokenType::LeftAngle, "<"),
                (TokenType::RightAngle, ">"),
            ],
        );
    }

    #[test]
    fn test_whitespace_and_comments() {
        let sources = SourceMap::new();
        let source = sources.add_file("test.fly", "  # comment\n  let".to_string());
        let mut lexer = Lexer::new(source);

        let token = lexer.advance();
        assert_eq!(token.ty, TokenType::Let);
        assert!(token.has_leading_whitespace);

        let eof_token = lexer.advance();
        assert_eq!(eof_token.ty, TokenType::Eof);
    }

    #[test]
    fn test_no_leading_whitespace() {
        let sources = SourceMap::new();
        let source = sources.add_file("test.fly", "let".to_string());
        let mut lexer = Lexer::new(source);

        let token = lexer.advance();
        assert_eq!(token.ty, TokenType::Let);
        assert!(!token.has_leading_whitespace);
    }

    #[test]
    fn test_multiple_tokens_leading_whitespace() {
        let sources = SourceMap::new();
        let source = sources.add_file("test.fly", "let a".to_string());
        let mut lexer = Lexer::new(source);

        let token1 = lexer.advance();
        assert_eq!(token1.ty, TokenType::Let);
        assert!(!token1.has_leading_whitespace);

        let token2 = lexer.advance();
        assert_eq!(token2.ty, TokenType::Identifier);
        assert!(token2.has_leading_whitespace);
    }

    #[test]
    #[ignore = "these should probably be disallowed"]
    fn test_invalid_tokens() {
        test_lexer(
            "0x 0b 123.",
            &[
                (TokenType::Error, "0"),
                (TokenType::Identifier, "x"),
                (TokenType::Error, "0"),
                (TokenType::Identifier, "b"),
                (TokenType::Integer, "123"),
                (TokenType::Period, "."),
            ],
        );
    }
}
