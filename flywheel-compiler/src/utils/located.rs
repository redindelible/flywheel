use crate::source::Location;

pub trait Located {
    fn location(&self) -> Location;
}

impl Located for Location {
    fn location(&self) -> Location {
        self.clone()
    }
}