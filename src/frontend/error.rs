use std::borrow::Cow;
use crate::frontend::source::Location;

#[derive(Debug, Clone, Hash)]
pub struct CompileError {
    kind: &'static str,
    description: Cow<'static, str>,
    location: Option<Location>
}

impl CompileError {
    pub fn with_description(kind: &'static str, description: impl Into<Cow<'static, str>>) -> Self {
        CompileError { kind, description: description.into(), location: None }
    }
    
    pub fn with_description_and_location(kind: &'static str, description: impl Into<Cow<'static, str>>, location: Location) -> Self {
        CompileError { kind, description: description.into(), location: Some(location) }
    }
}
