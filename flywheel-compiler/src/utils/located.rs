use std::ops::{Deref, DerefMut};
use crate::source::Location;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Located<T> {
    pub element: T,
    pub location: Location,
}

impl<T> Located<T> {
    pub fn locate(element: T, location: Location) -> Located<T> {
        Located { element, location }
    }
}

impl<T> Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl<T> DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.element
    }
}
