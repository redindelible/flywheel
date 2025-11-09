use std::borrow::Borrow;
use std::fmt::{Debug, Formatter};

use camino::{Utf8Path, Utf8PathBuf};
use flywheel_common::{ReservableMap, declare_key_type};
use triomphe::{Arc, ArcBorrow};

use crate::driver::Handle;
use crate::error::{CompileError, CompileResult};
use crate::query::Processor;

#[derive(Eq, PartialEq, Hash, Clone)]
pub enum SourceInput {
    AbsolutePath(Utf8PathBuf),
}

impl SourceInput {
    pub fn from_relative_path(anchor_source: &Source, relative_path: &Utf8Path) -> CompileResult<Self> {
        let Some(anchor_path) = anchor_source.absolute_path() else { todo!() };

        let new_path = anchor_path.parent().unwrap().join(relative_path);
        let Ok(normalized_path) = normpath::PathExt::normalize(new_path.as_std_path()) else {
            return Err(CompileError::with_description(
                "fs/could-not-open-file",
                format!("Could not open the file '{}'.", &new_path),
            ));
        };
        let Ok(utf8_path) = Utf8PathBuf::from_path_buf(normalized_path.into_path_buf()) else {
            return Err(CompileError::with_description(
                "fs/could-not-open-file",
                format!("Could not open the file '{}'.", &new_path),
            ));
        };
        let Ok(abs_path) = camino::absolute_utf8(utf8_path) else {
            return Err(CompileError::with_description(
                "fs/could-not-open-file",
                format!("Could not open the file '{}'.", &new_path),
            ));
        };

        Ok(SourceInput::AbsolutePath(abs_path))
    }
}

pub struct Sources {
    sources: parking_lot::RwLock<ReservableMap<SourceID, Arc<Source>>>,
}

impl Sources {
    pub fn new() -> Self {
        Sources { sources: parking_lot::RwLock::new(ReservableMap::new()) }
    }

    pub fn get_source<'a>(&'a self, source_id: SourceID) -> Option<ArcBorrow<'a, Source>> {
        let read_guard = self.sources.read();
        unsafe {
            Some(std::mem::transmute::<ArcBorrow<'_, Source>, ArcBorrow<'a, Source>>(
                read_guard.get(source_id)?.borrow_arc(),
            ))
        }
    }
}

impl Processor for Sources {
    type Input = SourceInput;
    type Output = SourceID;

    async fn process(handle: Handle, input: SourceInput) -> CompileResult<SourceID> {
        let (absolute_path, name, text) = match input {
            // SourceInput::String(text) => (None, "<string>".into(), text),
            SourceInput::AbsolutePath(absolute_path) => {
                let Ok(text) = tokio::fs::read_to_string(&absolute_path).await else {
                    return Err(CompileError::with_description(
                        "fs/could-not-open-file",
                        format!("Could not open the file '{}'.", &absolute_path),
                    ));
                };
                let name = absolute_path.file_stem().unwrap().to_owned();
                (Some(absolute_path), name, text)
            }
        };
        let interner = handle.interner();
        interner.add_buffer(text.clone());

        let this = handle.processor::<Sources>();

        let source_id = this.sources.write().add_with(|id| Arc::new(Source::new(id, absolute_path, name, text)));

        Ok(source_id)
    }
}
