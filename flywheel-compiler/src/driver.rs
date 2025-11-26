use std::ops::Deref;
use std::sync::Arc;
use rayon_core::{ThreadPool, ThreadPoolBuilder};
use flywheel_sources::{Interner, InternerState, SourceMap};

struct Inner {
    runtime: ThreadPool,
    interner_state: InternerState,
    sources: Arc<SourceMap>,
}

pub struct Driver(Handle);

impl Driver {
    pub fn new() -> Self {
        let runtime = ThreadPoolBuilder::new().build().unwrap();
        let sources = Arc::new(SourceMap::new());

        Driver(Handle {
            inner:
            Arc::new(Inner {
                runtime,
                interner_state: InternerState::new(Arc::clone(&sources)),
                sources,
            })
        })
    }
}

impl Deref for Driver {
    type Target = Handle;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone)]
pub struct Handle {
    inner: Arc<Inner>,
}

impl Handle {
    pub fn interner(&self) -> Interner {
        self.inner.interner_state.interner()
    }
}
