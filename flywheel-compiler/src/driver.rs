use std::future::Future;
use rayon_core::ThreadPool;
use flywheel_common::Interner;


struct Inner {
    runtime: ThreadPool,
}

pub struct Driver(Handle);

impl Driver {
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

#[derive(Clone)]
pub(super) struct Handle {
    inner: Arc<Inner>,
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
