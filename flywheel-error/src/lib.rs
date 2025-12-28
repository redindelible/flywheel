use std::fmt::{Display, Formatter};
use flywheel_sources::{LineInfo, SourceMap, Span};

pub type CompileResult<T> = Result<T, Box<CompileMessage>>;


#[derive(Debug)]
pub enum Level {
    Note,
    Error,
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Level::Note => "Note",
            Level::Error => "Error",
        })
    }
}

#[derive(Debug)]
pub struct CompileMessage {
    level: Level,
    message: String,
    span: Option<Span>,
    children: Vec<CompileMessage>,
}

impl CompileMessage {
    pub fn note(message: impl Into<String>) -> Self {
        CompileMessage { level: Level::Note, message: message.into(), span: None, children: vec![] }
    }

    pub fn error(message: impl Into<String>) -> Self {
        CompileMessage { level: Level::Error, message: message.into(), span: None, children: vec![] }
    }

    pub fn with_span(
        mut self,
        span: Span
    ) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_child(
        mut self,
        child: CompileMessage,
    ) -> Self {
        self.children.push(child);
        self
    }

    pub fn display<'a>(&'a self, sources: &'a SourceMap) -> CompileErrorWithDisplay<'a> {
        CompileErrorWithDisplay { level: 0, error: self, sources }
    }
}

pub struct CompileErrorWithDisplay<'a> {
    level: usize,
    error: &'a CompileMessage,
    sources: &'a SourceMap,
}

impl Display for CompileErrorWithDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent = " |  ".repeat(self.level);
        writeln!(f, "{}{}: {}", &indent, self.error.level, self.error.message)?;
        if let Some(span) = self.error.span {
            let LineInfo { source, text, line_index, span_start, span_end } = self.sources.get_span_line(span);
            let line_number =  format!("{: >3}", line_index + 1);
            writeln!(f, "{} {}--> {}", &indent, " ".repeat(line_number.len()), source.name())?;
            writeln!(f, "{}{} | {}", &indent, line_number, text)?;
            writeln!(f, "{}{}   {}{}", &indent,
                " ".repeat(line_number.len()),
                " ".repeat(span_start),
                "^".repeat(span_end - span_start),
            )?;
        }
        for child in &self.error.children {
            let error = CompileErrorWithDisplay { level: self.level + 1, error: child, sources: self.sources };
            error.fmt(f)?;
        } 
        Ok(())
    }
}
