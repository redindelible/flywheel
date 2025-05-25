use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use crate::id;
use crate::id::AstId;


/// A wrapper type that boxes it's contents, as well as providing an ID.
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Beacon<T> {
    id: AstId,
    internal: Box<T>,
}

impl<T: Display> Display for Beacon<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.internal)
    }
}

impl<T> Beacon<T> {
    pub fn new(from: T) -> Beacon<T> {
        let boxed = Box::new(from);
        Beacon { id: id::from_pointer(&boxed), internal:boxed }
    }
    
    pub fn as_ref(&self) -> &T {
        &self.internal
    }
    
    pub fn as_ref_mut(&mut self) -> &mut T {
        &mut self.internal
    }
    
    pub fn id(&self) -> AstId {
        self.id
    }
}

impl<T> Deref for Beacon<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.internal
    }
}

impl<T> DerefMut for Beacon<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.internal
    }
}