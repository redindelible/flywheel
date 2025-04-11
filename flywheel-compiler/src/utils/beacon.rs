use std::fmt::{Display, Formatter};
use crate::source::Location;
use crate::utils::located::Located;
use crate::utils::tag::Tag;

pub type Beacon<T> = Tag<Location, T>;
impl<T> Located for Beacon<T> {
    fn location(&self) -> Location {
        self.tag
    }
}
impl<T: Display> Display for Beacon<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.element)
    }
}

impl<T> Beacon<T> {
    pub fn new(element: T, location: Location) -> Beacon<T> {
        Beacon { tag: location, element: element }
    }
}