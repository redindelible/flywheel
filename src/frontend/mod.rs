pub mod ast;
mod parser;
mod lexer;
mod token;
mod source;

use std::collections::HashMap;
use std::sync::{Arc, OnceLock};

use parking_lot::{MappedRwLockReadGuard, Mutex, RwLock, RwLockReadGuard};
use dashmap::DashMap;
use string_interner::{backend::BufferBackend, StringInterner};

use crate::utils::ReservableKeyMap;

use crate::frontend::token::TokenType;
use crate::frontend::ast::FileAST;
use crate::frontend::lexer::LexerShared;
pub use crate::frontend::parser::ParseResult;
pub use crate::frontend::source::{Source, SourceID};

pub type InternedString = string_interner::symbol::SymbolU32;

pub struct FrontendDriver {
    sources: Mutex<ReservableKeyMap<SourceID, Arc<Source>>>,
    strings: Arc<StringsTable>,
    file_asts: DashMap<SourceID, OnceLock<ParseResult<Arc<FileAST>>>>
}

impl FrontendDriver {
    pub fn new() -> Self {
        FrontendDriver {
            sources: Mutex::new(ReservableKeyMap::new()),
            strings: Arc::new(StringsTable::new()),
            file_asts: DashMap::new()
        }
    }
    
    pub fn query_ast(&self, source_id: SourceID) -> ParseResult<Arc<FileAST>> {
        let cell_ref = match self.file_asts.get(&source_id) {
            Some(cell_ref) => cell_ref,
            None => self.file_asts.entry(source_id).or_default().downgrade()
        };
        cell_ref.get_or_init(|| {
            let source = Arc::clone(&self.sources.lock()[source_id]);
            parser::parse(Arc::clone(&self.strings), &source)
        }).clone()
    }

    pub fn add_string_source(&self, name: impl Into<String>, string: impl Into<String>) -> Arc<Source> {
        let mut sources = self.sources.lock();
        let key = sources.add_with(|id| Arc::new(Source::new(id, name.into(), string.into())));
        Arc::clone(&sources[key])
    }
}


struct StringsTable {
    symbols: RwLock<StringInterner<BufferBackend>>,
    keywords: HashMap<InternedString, TokenType>,
    lexer_shared: LexerShared,
}

impl StringsTable {
    fn new() -> Self {
        let mut inner = StringInterner::new();
        Self {
            keywords: HashMap::from_iter(TokenType::keywords().into_iter().map(|&(ty, text)| (inner.get_or_intern_static(text), ty))),
            symbols: RwLock::new(inner),
            lexer_shared: LexerShared::new(),
        }
    }

    fn resolve(&self, symbol: InternedString) -> Option<MappedRwLockReadGuard<'_, str>> {
        RwLockReadGuard::try_map(self.symbols.read(), |interner| interner.resolve(symbol)).ok()
    }

    fn get_or_intern(&self, text: &str) -> InternedString {
        let maybe_symbol = self.symbols.read().get(text);
        match maybe_symbol {
            Some(symbol) => symbol,
            None => self.symbols.write().get_or_intern(text)
        }
    }

    fn try_get_keyword(&self, symbol: InternedString) -> Option<TokenType> {
        self.keywords.get(&symbol).copied()
    }
}

