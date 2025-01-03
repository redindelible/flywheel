pub mod ast;
mod parser;
mod lexer;
mod token;
mod source;
mod error;

use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, OnceLock};
use camino::{Utf8Path, Utf8PathBuf};
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use dashmap::DashMap;
use string_interner::{backend::BufferBackend, StringInterner};

use crate::utils::ReservableKeyMap;

use token::TokenType;
use ast::FileAST;
pub use error::CompileError;
use lexer::LexerShared;
pub use source::{Source, SourceID};

pub type InternedString = string_interner::symbol::SymbolU32;

pub struct FrontendDriver {
    sources: RwLock<ReservableKeyMap<SourceID, Arc<Source>>>,
    files: DashMap<Utf8PathBuf, OnceLock<Result<SourceID, CompileError>>>,
    
    strings: Arc<StringsTable>,
    file_asts: DashMap<SourceID, OnceLock<Result<Arc<FileAST>, CompileError>>>
}

impl FrontendDriver {
    pub fn new() -> Self {
        FrontendDriver {
            sources: RwLock::new(ReservableKeyMap::new()),
            files: DashMap::new(),
            strings: Arc::new(StringsTable::new()),
            file_asts: DashMap::new()
        }
    }
    
    pub fn query_ast(&self, source_id: SourceID) -> Result<Arc<FileAST>, CompileError> {
        let cell_ref = match self.file_asts.get(&source_id) {
            Some(cell_ref) => cell_ref,
            None => self.file_asts.entry(source_id).or_default().downgrade()
        };
        cell_ref.get_or_init(|| {
            let source = Arc::clone(&self.sources.read()[source_id]);
            parser::parse(Arc::clone(&self.strings), &source)
        }).clone()
    }
    
    pub fn get_source(&self, source_id: SourceID) -> Option<MappedRwLockReadGuard<'_, Source>> {
        RwLockReadGuard::try_map(self.sources.read(), |map| map.get(source_id).map(Arc::as_ref)).ok()
    }

    pub fn add_file_source(&self, path: impl AsRef<Path>) -> Result<SourceID, CompileError> {
        let path = path.as_ref();
        let Some(path) = Utf8Path::from_path(path) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{:?}'.", path)));
        };
        let Ok(absolute_path) = camino::absolute_utf8(path) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", path)));
        };
        let cell_ref = match self.files.get(&absolute_path) {
            Some(cell_ref) => cell_ref,
            None => self.files.entry(absolute_path.clone()).or_default().downgrade()
        };
        cell_ref.get_or_init(|| {
            let Ok(text) = std::fs::read_to_string(&absolute_path) else {
                return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", path)));
            };
            let source_id = self.sources.write().add_with(|id| Arc::new(Source::new(id, absolute_path, path.as_str().into(), text)));
            Ok(source_id)
        }).clone()
    }

    pub fn add_string_source(&self, name: impl Into<String>, string: impl Into<String>) -> SourceID {
        self.sources.write().add_with(|id| Arc::new(Source::new_without_path(id, name.into(), string.into())))
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

