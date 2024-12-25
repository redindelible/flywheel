pub mod ast;
mod parser;
mod lexer;
mod token;
mod source;

use std::cell::{RefCell, Ref};
use std::collections::HashMap;
use std::sync::Arc;
use string_interner::{backend::BufferBackend, StringInterner};

use crate::utils::ReservableKeyMap;

use crate::frontend::token::TokenType;
use crate::frontend::ast::AST;
use crate::frontend::lexer::LexerShared;
pub use crate::frontend::parser::ParseResult;
pub use crate::frontend::source::{Source, SourceID};

pub type InternedString = string_interner::symbol::SymbolU32;

pub struct FrontendDriver {
    sources: ReservableKeyMap<SourceID, Arc<Source>>,
    strings: Arc<StringsTable>,
    asts: HashMap<SourceID, ParseResult<Arc<AST>>>
}

impl FrontendDriver {
    pub fn new() -> Self {
        FrontendDriver {
            sources: ReservableKeyMap::new(),
            strings: Arc::new(StringsTable::new()),
            asts: HashMap::new(),
        }
    }

    pub fn add_string_source(&mut self, string: impl Into<String>, name: String) -> Arc<Source> {
        let key = self.sources.add_with(|id| Arc::new(Source {
            id,
            text: string.into(),
            name
        }));
        self.sources[key].clone()
    }
    
    pub fn parse_source(&mut self, source_id: SourceID) -> ParseResult<Arc<AST>> {
        self.asts.entry(source_id).or_insert_with(|| {
            let source = self.sources[source_id].clone();
            parser::parse(self.strings.clone(), source)
        }).clone()
    }
}


struct StringsTable {
    symbols: RefCell<StringInterner<BufferBackend>>,
    keywords: HashMap<InternedString, TokenType>,
    lexer_shared: LexerShared,
}

impl StringsTable {
    fn new() -> Self {
        let mut inner = StringInterner::new();
        Self {
            keywords: HashMap::from_iter(TokenType::keywords().into_iter().map(|&(ty, text)| (inner.get_or_intern_static(text), ty))),
            symbols: RefCell::new(inner),
            lexer_shared: LexerShared::new(),
        }
    }

    fn resolve(&self, symbol: InternedString) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.symbols.borrow(), |interner| interner.resolve(symbol)).ok()
    }

    fn get_or_intern(&self, text: &str) -> InternedString {
        let maybe_symbol = self.symbols.borrow().get(text);
        match maybe_symbol {
            Some(symbol) => symbol,
            None => self.symbols.borrow_mut().get_or_intern(text)
        }
    }

    fn try_get_keyword(&self, symbol: InternedString) -> Option<TokenType> {
        self.keywords.get(&symbol).copied()
    }
}

