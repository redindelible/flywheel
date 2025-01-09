use std::borrow::Borrow;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::sync::OnceLock;
use camino::{Utf8Path, Utf8PathBuf};
use crate::utils::declare_key_type;

declare_key_type! {
    pub struct SourceID;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Location {
    pub(super) source: SourceID,
    pub(super) offset: u32,
    pub(super) length: u32
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
        write!(f, "Location {{ source: {:?}, range: {}..{} }}", self.source, self.offset, self.offset+self.length)
    }
}

pub struct Source {
    id: SourceID,
    absolute_path: Option<Utf8PathBuf>, 
    text: String,
    name: String,
    
    line_offsets: OnceLock<Box<[usize]>>
}

impl Source {
    pub fn new(id: SourceID, absolute_path: Utf8PathBuf, name: String, text: String) -> Self {
        Source { id, absolute_path: Some(absolute_path), name, text, line_offsets: OnceLock::new() }
    }

    pub fn new_without_path(id: SourceID, name: String, text: String) -> Self {
        Source { id, absolute_path: None, name, text, line_offsets: OnceLock::new() }
    }
    
    pub fn id(&self) -> SourceID { self.id }
    pub fn absolute_path(&self) -> Option<&Utf8Path> { self.absolute_path.as_ref().map(Utf8PathBuf::as_path) }
    pub fn name(&self) -> &str { &self.name }
    pub fn text(&self) -> &str { &self.text }
    
    fn fill_line_offsets(&self) -> &[usize] {
        use std::iter::once;
        self.line_offsets.get_or_init(|| {
            memchr::memchr_iter(b'\n', self.text.as_bytes()).chain(once(self.text.len())).collect()
        })
    }
    
    pub fn get_line(&self, offset: usize, length: usize) -> Option<(&str, usize, Range<usize>)> {
        if offset > self.text.len() {
            return None;
        }
        let line_offsets = self.fill_line_offsets();
        let line_index = line_offsets.binary_search(&offset).unwrap_or_else(|n| n);
        let (start_offset, end_offset) = match line_index {
            0 => (0, line_offsets[0]),
            line_offsets_index => (line_offsets[line_offsets_index-1]+1, line_offsets[line_offsets_index])
        };
        let start = offset - start_offset;
        if start + length > end_offset + 1 {
            return None;
        }
        Some((&self.text[start_offset..end_offset], line_index, start..start+length))
    }
}
