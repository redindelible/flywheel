use std::borrow::Cow;

use flywheel_sources::Span;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    description: Cow<'static, str>,
    span: Option<Span>,
}

impl CompileError {
    pub fn with_description(_kind: &'static str, description: impl Into<Cow<'static, str>>) -> Self {
        CompileError { description: description.into(), span: None }
    }

    pub fn with_description_and_location(
        _kind: &'static str,
        description: impl Into<Cow<'static, str>>,
        location: Span,
    ) -> Self {
        CompileError {
            description: description.into(), 
            span: Some(location),
        }
    }
    
    pub fn description(&self) -> &str {
        &*self.description
    }

    // pub fn display<'a>(&'a self, frontend: &'a FrontendDriver) -> CompileErrorWithHandle<'a> {
    //     self.display_from_handle(frontend.get_handle())
    // }
    // 
    // fn display_from_handle<'a>(&'a self, handle: &'a Handle) -> CompileErrorWithHandle<'a> {
    //     CompileErrorWithHandle { error: &self.inner, handle }
    // }
}

// pub struct CompileErrorWithHandle<'a> {
//     error: &'a CompileErrorInner,
//     handle: &'a Handle,
// }
// 
// impl Debug for CompileErrorWithHandle<'_> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self.error)
//     }
// }
// 
// impl Display for CompileErrorWithHandle<'_> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         writeln!(f, "Error: {}", &self.error.description)?;
//         if let Some(location) = self.error.location {
//             let source = self.handle.get_source(location.source);
//             let offset = if location.offset == u32::MAX { source.text().len() } else { location.offset as usize };
//             let (line, line_index, range) = source.get_line(offset, location.length as usize).unwrap();
//             let line_number = (line_index + 1).to_string();
//             let padded_size = line_number.len() + 1;
//             writeln!(f, "{:pad$}--> {name}", "", pad = padded_size, name = source.name())?;
//             writeln!(f, "{no:>pad$} | {line}", no = line_number, pad = padded_size, line = line)?;
//             writeln!(
//                 f,
//                 "{:pad$}   {:off$}{:^<len$}",
//                 "",
//                 "",
//                 "",
//                 pad = padded_size,
//                 off = range.start,
//                 len = range.len()
//             )?;
//         }
//         Ok(())
//     }
// }
