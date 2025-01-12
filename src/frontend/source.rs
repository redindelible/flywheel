use std::borrow::Borrow;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::sync::OnceLock;

use camino::{Utf8Path, Utf8PathBuf};
use triomphe::{Arc, ArcBorrow};

use crate::frontend::driver::Handle;
use crate::frontend::error::{CompileError, CompileResult};
use crate::frontend::query::Processor;
use crate::utils::{ReservableMap, declare_key_type};

declare_key_type! {
    pub struct SourceID;
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub enum SourceInput {
    String(Arc<str>),
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

    pub fn get_source(&self, source_id: SourceID) -> ArcBorrow<'_, Source> {
        let read_guard = self.sources.read();
        unsafe {
            std::mem::transmute::<ArcBorrow<'_, Source>, ArcBorrow<'_, Source>>(
                read_guard.get(source_id).unwrap().borrow_arc(),
            )
        }
    }
}

impl Processor for Sources {
    type Input = SourceInput;
    type Output = SourceID;

    async fn process(handle: Handle, input: SourceInput) -> CompileResult<SourceID> {
        let (absolute_path, name, text) = match input {
            SourceInput::String(text) => (None, "<string>".into(), text),
            SourceInput::AbsolutePath(absolute_path) => {
                let Ok(text) = tokio::fs::read_to_string(&absolute_path).await else {
                    return Err(CompileError::with_description(
                        "fs/could-not-open-file",
                        format!("Could not open the file '{}'.", &absolute_path),
                    ));
                };
                let name = absolute_path.file_stem().unwrap().to_owned();
                (Some(absolute_path), name, text.into())
            }
        };
        let interner = handle.interner();
        interner.add_buffer(text.clone());

        let this = handle.processor::<Sources>();

        let source_id = this.sources.write().add_with(|id| Arc::new(Source::new(id, absolute_path, name, text)));

        Ok(source_id)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Location {
    pub(super) source: SourceID,
    pub(super) offset: u32,
    pub(super) length: u32,
}

impl Location {
    pub fn combine(&self, other: impl Borrow<Location>) -> Location {
        let other = *other.borrow();
        assert_eq!(self.source, other.source);
        let start = self.offset.min(other.offset);
        let end = (self.offset + self.length).max(other.offset + other.length);
        Location { source: self.source, offset: start, length: end - start }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location {{ source: {:?}, range: {}..{} }}", self.source, self.offset, self.offset + self.length)
    }
}

pub struct Source {
    id: SourceID,
    absolute_path: Option<Utf8PathBuf>,
    text: Arc<str>,
    name: String,

    line_offsets: OnceLock<Box<[usize]>>,
}

impl Source {
    fn new(id: SourceID, absolute_path: Option<Utf8PathBuf>, name: String, text: Arc<str>) -> Self {
        Source { id, absolute_path, name, text, line_offsets: OnceLock::new() }
    }

    pub fn id(&self) -> SourceID {
        self.id
    }
    pub fn absolute_path(&self) -> Option<&Utf8Path> {
        self.absolute_path.as_ref().map(Utf8PathBuf::as_path)
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn text(&self) -> &str {
        &self.text
    }

    fn fill_line_offsets(&self) -> &[usize] {
        use std::iter::once;
        self.line_offsets
            .get_or_init(|| memchr::memchr_iter(b'\n', self.text.as_bytes()).chain(once(self.text.len())).collect())
    }

    pub fn get_line(&self, offset: usize, length: usize) -> Option<(&str, usize, Range<usize>)> {
        if offset > self.text.len() {
            return None;
        }
        let line_offsets = self.fill_line_offsets();
        let line_index = line_offsets.binary_search(&offset).unwrap_or_else(|n| n);
        let (start_offset, end_offset) = match line_index {
            0 => (0, line_offsets[0]),
            line_offsets_index => (line_offsets[line_offsets_index - 1] + 1, line_offsets[line_offsets_index]),
        };
        let start = offset - start_offset;
        if start + length > end_offset + 1 {
            return None;
        }
        Some((&self.text[start_offset..end_offset], line_index, start..start + length))
    }
}
