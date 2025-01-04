pub mod ast;
mod parser;
mod lexer;
mod token;
mod source;
mod error;

use std::collections::HashMap;
use std::future::Future;
use std::path::Path;
use std::sync::Arc;
use camino::{Utf8Path, Utf8PathBuf};
use tokio::runtime::Runtime;
use tokio::task::JoinHandle;
use crate::utils::{Interner, InternedString, OnceMap, ReservableKeyMap};

use token::TokenType;
use ast::FileAST;
pub use error::CompileError;
use lexer::LexerShared;
pub use source::{Source, SourceID};

pub type CompileResult<T> = Result<T, CompileError>;

pub struct FrontendDriver {
    runtime: Runtime,

    sources: parking_lot::RwLock<ReservableKeyMap<SourceID, Arc<Source>>>,
    strings: Arc<StringsTable>,

    files: OnceMap<Utf8PathBuf, CompileResult<SourceID>>,
    asts: OnceMap<SourceID, CompileResult<Arc<FileAST>>>
}

impl FrontendDriver {
    pub fn new() -> Self {
        FrontendDriver {
            runtime: Runtime::new().unwrap(),
            sources: parking_lot::RwLock::new(ReservableKeyMap::new()),
            strings: Arc::new(StringsTable::new()),
            files: OnceMap::new(),
            asts: OnceMap::new()
        }
    }

    pub fn spawn<F: Future + Send + 'static>(&self, task: F) -> JoinHandle<F::Output> where F::Output: Send + 'static {
        self.runtime.spawn(task)
    }
    
    pub fn block_on<F: Future>(&self, fut: F) -> F::Output {
        self.runtime.block_on(fut)
    }

    pub fn get_source(&self, source_id: SourceID) -> Option<Arc<Source>> {
        self.sources.read().get(source_id).cloned()
    }

    pub async fn query_file_source(&self, path: impl AsRef<Path>) -> Result<SourceID, CompileError> {
        let path = path.as_ref();
        let Some(path) = Utf8Path::from_path(path) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{:?}'.", path)));
        };
        let Ok(absolute_path) = camino::absolute_utf8(path) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", path)));
        };
        self.files.get_or_init(absolute_path.clone(), || async {
            let Ok(text) = tokio::fs::read_to_string(&absolute_path).await else {
                return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", path)));
            };
            let source_id = self.sources.write().add_with(|id| Arc::new(Source::new(id, absolute_path, path.as_str().into(), text)));
            Ok(source_id)
        }).await
    }

    #[allow(dead_code)]
    pub fn add_string_source(&self, name: impl Into<String>, string: impl Into<String>) -> SourceID {
        self.sources.write().add_with(|id| Arc::new(Source::new_without_path(id, name.into(), string.into())))
    }
    
    pub async fn query_ast(&self, source_id: SourceID) -> Result<Arc<FileAST>, CompileError> {
        self.asts.get_or_init(source_id, || async {
            let source = Arc::clone(&self.sources.read()[source_id]);
            parser::parse(Arc::clone(&self.strings), &source)
        }).await
    }
}


struct StringsTable {
    symbols: Interner,
    keywords: HashMap<InternedString, TokenType>,
    lexer_shared: LexerShared,
}

impl StringsTable {
    fn new() -> Self {
        let symbols = Interner::new();
        Self {
            keywords: HashMap::from_iter(TokenType::keywords().into_iter().map(|&(ty, text)| (symbols.get_or_intern_static(text), ty))),
            symbols,
            lexer_shared: LexerShared::new(),
        }
    }

    fn resolve(&self, symbol: InternedString) -> Option<parking_lot::MappedRwLockReadGuard<'_, str>> {
        self.symbols.resolve(symbol)
    }

    fn get_or_intern(&self, text: &str) -> InternedString {
        self.symbols.get_or_intern(text)
    }

    fn try_get_keyword(&self, symbol: InternedString) -> Option<TokenType> {
        self.keywords.get(&symbol).copied()
    }
}

