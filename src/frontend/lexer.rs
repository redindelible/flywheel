use std::collections::HashMap;

use regex_automata::meta::Regex;
use regex_automata::{Anchored, Input, Span};
use triomphe::{Arc, ArcBorrow};

use crate::frontend::source::{Location, SourceID};
use crate::frontend::token::{Token, TokenStream, TokenType};
use crate::utils::{InternedString, Interner};

#[derive(Debug)]
pub struct LexerError {
    loc: Location,
}

enum PatternType {
    Whitespace,
    Comment,
    String,
    Interned(TokenType),
    Basic(TokenType),
}

const PATTERNS: &[(&str, PatternType)] = &[
    (r"[ \t\r\n]+", PatternType::Whitespace),
    (r"#[^\n]+", PatternType::Comment),
    (r"[_\p{ID_Start}][_\p{ID_Continue}]*", PatternType::Interned(TokenType::Identifier)),
    (r#""([^"\\]|\\.)*""#, PatternType::String),
    (r"0b[0-1]+", PatternType::Interned(TokenType::Binary)),
    (r"0x[a-fA-F0-9]+", PatternType::Interned(TokenType::Hexadecimal)),
    (r"[0-9]+", PatternType::Interned(TokenType::Integer)),
    (r"[0-9]+\.[0-9]+", PatternType::Interned(TokenType::Float)),
    (r"\->", PatternType::Basic(TokenType::LeftArrow)),
    (r"\.", PatternType::Basic(TokenType::Period)),
    (r"\,", PatternType::Basic(TokenType::Comma)),
    (r"\:", PatternType::Basic(TokenType::Colon)),
    (r"\;", PatternType::Basic(TokenType::Semicolon)),
    (r"\=", PatternType::Basic(TokenType::Equal)),
    (r"\+", PatternType::Basic(TokenType::Plus)),
    (r"\-", PatternType::Basic(TokenType::Minus)),
    (r"\*", PatternType::Basic(TokenType::Star)),
    (r"\/", PatternType::Basic(TokenType::Slash)),
    (r"\%", PatternType::Basic(TokenType::Percent)),
    (r"\{", PatternType::Basic(TokenType::LeftBrace)),
    (r"\}", PatternType::Basic(TokenType::RightBrace)),
    (r"\(", PatternType::Basic(TokenType::LeftParenthesis)),
    (r"\)", PatternType::Basic(TokenType::RightParenthesis)),
    (r"\[", PatternType::Basic(TokenType::LeftBracket)),
    (r"\]", PatternType::Basic(TokenType::RightBracket)),
    (r"<", PatternType::Basic(TokenType::LeftAngle)),
    (r">", PatternType::Basic(TokenType::RightAngle)),
];

#[derive(Clone)]
pub(super) struct LexerShared {
    interner: Arc<Interner>,
    regex: Regex,
    keywords: HashMap<InternedString, TokenType>,
}

impl LexerShared {
    pub fn new(interner: &Arc<Interner>) -> LexerShared {
        let patterns = PATTERNS.iter().map(|item| item.0).collect::<Vec<&'static str>>();
        let regex = Regex::new_many(&patterns).unwrap();
        LexerShared {
            interner: interner.clone(),
            regex,
            keywords: HashMap::from_iter(
                TokenType::keywords().iter().map(|&(ty, text)| (interner.get_or_intern_static(text), ty)),
            ),
        }
    }

    pub fn interner(&self) -> ArcBorrow<'_, Interner> {
        self.interner.borrow_arc()
    }
}

pub(super) struct Lexer<'a> {
    interner: &'a Interner,
    keywords: &'a HashMap<InternedString, TokenType>,
    source_id: SourceID,
    regex: Regex,
    text: &'a str,
    span: Span,
}

impl<'a> Lexer<'a> {
    pub fn new(shared: &'a LexerShared, source_id: SourceID, text: &'a str) -> Lexer<'a> {
        let span = Input::new(text).get_span();

        Lexer {
            interner: &shared.interner,
            keywords: &shared.keywords,
            source_id,
            regex: shared.regex.clone(),
            text,
            span,
        }
    }

    fn lex_next(&mut self) -> Option<Token> {
        self.try_lex_next().map(|maybe_token| {
            maybe_token.unwrap_or_else(|error| Token {
                ty: TokenType::Error,
                text: None,
                has_leading_whitespace: false,
                loc: error.loc,
            })
        })
    }

    fn try_lex_next(&mut self) -> Option<Result<Token, LexerError>> {
        let mut input = Input::new(self.text).anchored(Anchored::Yes);
        input.set_span(self.span);

        let mut has_leading_whitespace = false;
        while input.start() < input.haystack().len() {
            if let Some(match_) = self.regex.search(&input) {
                let str = &self.text[match_.range()];
                let loc =
                    Location { source: self.source_id, offset: input.start() as u32, length: match_.len() as u32 };
                input.set_start(match_.end());

                match PATTERNS[match_.pattern().as_usize()].1 {
                    PatternType::Whitespace | PatternType::Comment => {
                        has_leading_whitespace = true;
                    }
                    PatternType::String => {
                        let symbol = self.interner.get_or_intern_in_buffer(&str[1..str.len() - 1]);
                        self.span = input.get_span();
                        return Some(Ok(Token {
                            ty: TokenType::String,
                            text: Some(symbol),
                            has_leading_whitespace,
                            loc,
                        }));
                    }
                    PatternType::Interned(ty_fn) => {
                        let symbol = self.interner.get_or_intern_in_buffer(str);
                        let ty = self.keywords.get(&symbol).copied().unwrap_or(ty_fn);
                        self.span = input.get_span();
                        return Some(Ok(Token { ty, text: Some(symbol), has_leading_whitespace, loc }));
                    }
                    PatternType::Basic(ty) => {
                        self.span = input.get_span();
                        return Some(Ok(Token { ty, text: None, has_leading_whitespace, loc }));
                    }
                }
            } else {
                return Some(Err(LexerError {
                    loc: Location { source: self.source_id, offset: input.start() as u32, length: 1 },
                }));
            }
        }

        None
    }
}

impl TokenStream for Lexer<'_> {
    fn next(&mut self) -> Option<Token> {
        self.lex_next()
    }

    fn source_id(&self) -> SourceID {
        self.source_id
    }

    fn interner(&self) -> &Interner {
        self.interner
    }
}
