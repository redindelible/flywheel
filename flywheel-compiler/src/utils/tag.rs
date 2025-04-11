use std::ops::{Deref, DerefMut};

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Tag<Ty, Over> {
    pub tag: Ty,
    pub element: Over,
}

impl<Ty, Over> Deref for Tag<Ty, Over> {
    type Target = Over;

    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl<Ty, Over> DerefMut for Tag<Ty, Over> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.element
    }
}
