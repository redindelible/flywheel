pub mod ast;
mod parser;
mod lexer;
mod token;
mod source;
mod error;
mod type_check;

use std::collections::HashMap;
use std::future::Future;
use std::sync::Arc;
use std::time::Instant;
use camino::{Utf8Path, Utf8PathBuf};
use futures::FutureExt;
use tokio::runtime::Runtime;
use crate::utils::{Interner, InternedString, OnceMap, ReservableMap};

use token::TokenType;
use ast::FileAST;
pub use error::CompileError;
use lexer::LexerShared;
pub use source::{Source, SourceID};
use crate::frontend::type_check::CollectImports;

pub type CompileResult<T> = Result<T, CompileError>;

pub struct FrontendDriver {
    inner: Arc<Inner>
}

#[derive(Clone)]
pub struct Handle {
    inner: Arc<Inner>
}

struct Inner {
    runtime: Runtime,

    sources: parking_lot::RwLock<ReservableMap<SourceID, Arc<Source>>>,
    strings: Arc<StringsTable>,

    files: OnceMap<Utf8PathBuf, CompileResult<SourceID>>,
    asts: OnceMap<SourceID, CompileResult<Arc<FileAST>>>,
    collected_imports: OnceMap<SourceID, CompileResult<Arc<CollectImports>>>
}

impl FrontendDriver {
    pub fn new() -> Self {
        let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();
        
        FrontendDriver { 
            inner: Arc::new(Inner {
                runtime,
                sources: parking_lot::RwLock::new(ReservableMap::new()),
                strings: Arc::new(StringsTable::new()),
                files: OnceMap::new(),
                asts: OnceMap::new(),
                collected_imports: OnceMap::new()
            }) 
        }
    }
    
    pub fn handle(&self) -> Handle {
        Handle { inner: self.inner.clone() }
    }

    pub fn block_on<F: Future>(&self, fut: F) -> F::Output {
        self.inner.runtime.block_on(fut)
    }
}

impl Handle {
    pub fn strings(&self) -> &StringsTable {
        &self.inner.strings
    }
    
    pub fn get_source(&self, source_id: SourceID) -> Option<Arc<Source>> {
        self.inner.sources.read().get(source_id).cloned()
    }

    pub async fn query_relative_source(&self, anchor: SourceID, relative_path: &Utf8Path) -> CompileResult<SourceID> {
        let anchor_source = self.get_source(anchor).unwrap();
        let Some(anchor_path) = anchor_source.absolute_path() else { todo!() };
        
        let new_path = anchor_path.parent().unwrap().join(relative_path);
        let Ok(normalized_path) = normpath::PathExt::normalize(new_path.as_std_path()) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", &new_path)));
        };
        let Ok(utf8_path) = Utf8PathBuf::from_path_buf(normalized_path.into_path_buf()) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", &new_path)));
        };
        self.query_file_source(utf8_path).await
    } 
    
    pub async fn query_file_source(&self, path: Utf8PathBuf) -> CompileResult<SourceID> {
        let Ok(absolute_path) = camino::absolute_utf8(&path) else {
            return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", &path)));
        };
        self.inner.files.get_or_init(absolute_path.clone(), || {
            let handle = self.clone();
            self.inner.runtime.spawn(async move {
                let Ok(text) = tokio::fs::read_to_string(&absolute_path).await else {
                    return Err(CompileError::with_description("fs/could-not-open-file", format!("Could not open the file '{}'.", &path)));
                };
                let source_id = handle.inner.sources.write().add_with(|id| Arc::new(Source::new(id, absolute_path, path.as_str().into(), text)));
                Ok(source_id)
            }).map(Result::unwrap)
        }).await
    }

    #[allow(dead_code)]
    pub fn add_string_source(&self, name: String, string: String) -> SourceID {
        self.inner.sources.write().add_with(|id| Arc::new(Source::new_without_path(id, name, string)))
    }

    pub async fn query_ast(&self, source_id: SourceID) -> CompileResult<Arc<FileAST>> {
        let ast_or_error = self.inner.asts.get_or_init(source_id, || {
            let handle = self.clone();
            let fut = self.inner.runtime.spawn(async move {
                let source = handle.get_source(source_id).unwrap();
                let string_table = Arc::clone(&handle.inner.strings);
                parser::parse(string_table, &source)
            }).map(Result::unwrap);
            fut
        }).await;
        ast_or_error
    }

    pub async fn query_collected_imports(&self, source_id: SourceID) -> CompileResult<Arc<CollectImports>> {
        self.inner.collected_imports.get_or_init(source_id, || {
            let handle = self.clone();
            self.inner.runtime.spawn(async move {
                CollectImports::process(handle, source_id).await.map(Arc::new)
            }).map(Result::unwrap)
        }).await
    }
}


pub struct StringsTable {
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

