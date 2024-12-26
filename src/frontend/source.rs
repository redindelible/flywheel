use std::borrow::Borrow;
use std::fmt::{Debug, Formatter};
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
    pub fn new_file(source: SourceID) -> Location {
        Location { source, offset: 0, length: 0 }
    }
    
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
        write!(f, "Location {{ source: {}, range: {}..{} }}", self.source.0.to_u32(), self.offset, self.offset+self.length)
    }
}

pub struct Source {
    pub(super) id: SourceID,
    pub(super) text: String,
    pub(super) name: String
}

impl Source {
    pub fn id(&self) -> SourceID { self.id }
    pub fn name(&self) -> &str { &self.name }
    pub fn text(&self) -> &str { &self.text }
}
