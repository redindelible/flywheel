use std::borrow::Cow;
use std::fmt::{Debug, Display, Formatter};
use crate::frontend::Handle;
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

    pub fn display<'a>(&'a self, handle: &'a Handle) -> CompileErrorWithHandle<'a> {
        CompileErrorWithHandle { error: self, handle }
    }
}

pub struct CompileErrorWithHandle<'a> {
    error: &'a CompileError,
    handle: &'a Handle,
}

impl<'a> Debug for CompileErrorWithHandle<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.error)
    }
}

impl<'a> Display for CompileErrorWithHandle<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}\n", &self.error.description)?;
        if let Some(location) = self.error.location {
            let source = self.handle.get_source(location.source).unwrap();
            let (line, line_index, range) = source.get_line(location.offset as usize, location.length as usize).unwrap();
            let line_number = (line_index + 1).to_string();
            let padded_size = line_number.len() + 1;
            write!(f, "{:pad$}{arrow} {name}\n", "", pad=padded_size, arrow="-->", name=source.name())?;
            write!(f, "{no:>pad$} | {line}\n", no=line_number, pad=padded_size, line=line)?;
            write!(f, "{:pad$}   {:off$}{:^<len$}\n", "", "", "", pad=padded_size, off=range.start, len=range.len())?;
        }
        Ok(())
    }
}
