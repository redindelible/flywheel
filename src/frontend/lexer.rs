use std::sync::Arc;

use regex_automata::{Input, meta::Regex, Anchored, Span};

use crate::frontend::{
    StringsTable,
    source::{Location, SourceID},
    token::{Token, TokenType}
};
use crate::frontend::source::Source;

#[derive(Debug)]
pub struct LexerError {
    loc: Location
}

enum PatternType {
    Whitespace,
    Comment,
    Interned(TokenType),
    Basic(TokenType)
}

const PATTERNS: &'static [(&'static str, PatternType)] = &[
    (r"[ \t\r\n]+", PatternType::Whitespace),
    (r"#[^\n]+", PatternType::Comment),
    (r"[_\p{ID_Start}][_\p{ID_Continue}]*", PatternType::Interned(TokenType::Identifier)),
    (r"0b[0-1]+", PatternType::Interned(TokenType::Binary)),
    (r"0x[a-fA-F0-9]+", PatternType::Interned(TokenType::Hexadecimal)),
    (r"[0-9]+", PatternType::Interned(TokenType::Integer)),
    (r"[0-9]+\.[0-9]+", PatternType::Interned(TokenType::Float)),
    (r"\->", PatternType::Basic(TokenType::LeftArrow)),
    (r"\.", PatternType::Basic(TokenType::Period)),
    (r"\,", PatternType::Basic(TokenType::Comma)),
    (r"\:", PatternType::Basic(TokenType::Colon)),
    (r"\;", PatternType::Basic(TokenType::Semicolon)),
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
    regex: Regex
}

impl LexerShared {
    pub fn new() -> LexerShared {
        let patterns = PATTERNS.iter().map(|item| item.0).collect::<Vec<&'static str>>();
        let regex = Regex::new_many(&patterns).unwrap();
        LexerShared { regex }
    }
}

pub(super) struct Lexer<'a> {
    context: &'a StringsTable,
    source_id: SourceID,
    regex: Regex,
    source_arc: Arc<Source>,
    span: Span
}

impl<'a> Lexer<'a> {
    pub fn new(context: &'a StringsTable, source_arc: Arc<Source>) -> Lexer<'a> {
        let span = Input::new(&source_arc.text).get_span();

        Lexer {
            context,
            source_id: source_arc.id(),
            regex: context.lexer_shared.regex.clone(),
            source_arc, 
            span,
        }
    }

    pub fn context(&self) -> &'a StringsTable {
        self.context
    }

    pub fn source_id(&self) -> SourceID {
        self.source_id
    }
    
    fn lex_next(&mut self) -> Option<Token> {
        self.try_lex_next().map(|maybe_token| {
            maybe_token.unwrap_or_else(|error| {
                Token { ty: TokenType::Error, text: None, has_leading_whitespace: false, loc: error.loc }
            })
        })
    }
    
    fn try_lex_next(&mut self) -> Option<Result<Token, LexerError>> {
        let mut input = Input::new(&self.source_arc.text).anchored(Anchored::Yes);
        input.set_span(self.span);

        let mut has_leading_whitespace = false;
        while input.start() < input.haystack().len() {
            if let Some(match_) = self.regex.search(&input) {
                input.set_start(match_.end());
                let str = &self.source_arc.text[match_.range()];
                let loc = Location { source: self.source_id, offset: input.start() as u32, length: match_.len() as u32 };

                match PATTERNS[match_.pattern().as_usize()].1 {
                    PatternType::Whitespace | PatternType::Comment => {
                        has_leading_whitespace = true;
                    },
                    PatternType::Interned(ty_fn) => {
                        let symbol = self.context.get_or_intern(str);
                        let ty = self.context.try_get_keyword(symbol).unwrap_or(ty_fn);
                        self.span = input.get_span();
                        return Some(Ok(Token { ty, text: Some(symbol), has_leading_whitespace, loc }));
                    },
                    PatternType::Basic(ty) => {
                        self.span = input.get_span();
                        return Some(Ok(Token { ty, text: None, has_leading_whitespace, loc }));
                    }
                }
            } else {
                return Some(Err(LexerError { loc: Location { source: self.source_id, offset: input.start() as u32, length: 1 } }));
            }
        }
        
        None
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_next()
    }
}
