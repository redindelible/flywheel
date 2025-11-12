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
        let eof = source.span(text.len()..text.len()+1);
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
                span: self.source.span(span),
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

    pub fn position(&self) -> usize {
        self.inner.as_ref().map_or(self.source.text().len(), |lexer| lexer.span().end)
    }
    
    pub fn source(&self) -> &'a Source {
        self.source
    }
}
