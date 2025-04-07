use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use crate::source::Location;

pub trait Located {
    fn location(&self) -> Location;
}

impl Located for Location {
    fn location(&self) -> Location {
        self.clone()
    }
}
impl<T> Located for Beacon<T> {
    fn location(&self) -> Location {
        self.location
    }
}

/// A wrapper struck for the type `T` that attaches a location to it.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Beacon<T> {
    pub element: T,
    pub location: Location,
}

impl<T: Display> Display for Beacon<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.element)
    }
}

impl<T> Beacon<T> {
    pub fn new(element: T, location: Location) -> Beacon<T> {
        Beacon { element, location }
    }
}

impl<T> Deref for Beacon<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl<T> DerefMut for Beacon<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.element
    }
}
