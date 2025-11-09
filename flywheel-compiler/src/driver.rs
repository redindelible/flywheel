use std::future::Future;

use camino::Utf8PathBuf;
use flywheel_common::Interner;
use tokio::runtime::Runtime;
use tokio::task::JoinHandle;
use triomphe::{Arc, ArcBorrow};

use crate::error::CompileResult;
use crate::parser::Parse;
use crate::source::{Source, SourceID, SourceInput, Sources};
use crate::type_check::{ComputeDeclaredNames, ComputeDefinedTypes, DefinedTypes};

pub struct FrontendDriver(Handle);

#[derive(Clone)]
pub(super) struct Handle {
    inner: Arc<Inner>,
}

struct Inner {
    runtime: Runtime,

    interner: Arc<Interner>,
}

impl FrontendDriver {
    pub fn new() -> Self {
        let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();

        let interner = Arc::new(Interner::new());
        let parse = Parse::new(&interner);
        let sources = Sources::new();
        let defined_types = ComputeDefinedTypes::new(&interner);

        FrontendDriver(Handle { inner: Arc::new(Inner { runtime, interner, query_engine }) })
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
}

impl Default for FrontendDriver {
    fn default() -> Self {
        FrontendDriver::new()
    }
}
