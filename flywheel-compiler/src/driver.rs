use std::future::Future;

use camino::Utf8PathBuf;
use flywheel_common::Interner;
use tokio::runtime::Runtime;
use tokio::task::JoinHandle;
use triomphe::{Arc, ArcBorrow};

use crate::error::CompileResult;
use crate::parser::Parse;
use crate::query::{Processor, QueryEngine, SupportsQueryOn, query_engine};
use crate::source::{Source, SourceID, SourceInput, Sources};
use crate::type_check::{ComputeDeclaredNames, ComputeDefinedTypes, DefinedTypes};

pub struct FrontendDriver(Handle);

#[derive(Clone)]
pub(super) struct Handle {
    inner: Arc<Inner>,
}

query_engine! {
    pub(super) struct CompilerQueryEngine {
        parse: Parse,
        sources: Sources,
        declared_names: ComputeDeclaredNames,
        defined_types: ComputeDefinedTypes,
    }
}

struct Inner {
    runtime: Runtime,

    interner: Arc<Interner>,
    query_engine: CompilerQueryEngine,
}

impl FrontendDriver {
    pub fn new() -> Self {
        let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();

        let interner = Arc::new(Interner::new());
        let parse = Parse::new(&interner);
        let sources = Sources::new();
        let defined_types = ComputeDefinedTypes::new(&interner);
        let query_engine = CompilerQueryEngine::new(parse, sources, ComputeDeclaredNames, defined_types);

        FrontendDriver(Handle { inner: Arc::new(Inner { runtime, interner, query_engine }) })
    }

    pub async fn query_file_source(&self, path: impl Into<Utf8PathBuf>) -> CompileResult<SourceID> {
        self.0.query::<Sources>(SourceInput::AbsolutePath(path.into())).await.map(|source| *source)
    }

    pub async fn query_defined_types(&self, source_id: SourceID) -> CompileResult<ArcBorrow<'_, DefinedTypes>> {
        self.0.query::<ComputeDefinedTypes>(source_id).await
    }

    pub fn block_on<F: Future>(&self, fut: F) -> F::Output {
        self.0.inner.runtime.block_on(fut)
    }

    pub(super) fn get_handle(&self) -> &Handle {
        &self.0
    }
}

impl Handle {
    pub(super) fn spawn<Fut>(&self, fut: Fut) -> JoinHandle<Fut::Output>
    where
        Fut: Future + Send + 'static,
        Fut::Output: Send + 'static,
    {
        self.inner.runtime.spawn(fut)
    }

    pub(super) fn interner(&self) -> &Interner {
        &self.inner.interner
    }

    pub(super) fn get_source(&self, source_id: SourceID) -> ArcBorrow<'_, Source> {
        self.inner.query_engine.get_processor::<Sources>().get_source(source_id)
    }

    pub(super) fn processor<P>(&self) -> &P
    where
        P: Processor,
        CompilerQueryEngine: SupportsQueryOn<P>,
    {
        self.inner.query_engine.get_processor()
    }

    pub(super) async fn query<P: Processor>(&self, input: P::Input) -> CompileResult<ArcBorrow<'_, P::Output>>
    where
        CompilerQueryEngine: SupportsQueryOn<P>,
    {
        self.inner.query_engine.query(self, input).await
    }
}

impl Default for FrontendDriver {
    fn default() -> Self {
        FrontendDriver::new()
    }
}
