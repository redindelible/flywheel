use std::fmt::{Display, Formatter};
use flywheel_sources::{LineInfo, SourceMap, Span};

pub type CompileResult<T> = Result<T, Box<CompileMessage>>;


#[derive(Debug)]
pub enum Level {
    Error
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Level::Error => "Error",
        })
    }
}

#[derive(Debug)]
pub struct CompileMessage {
    level: Level,
    message: String,
    span: Option<Span>,
}

impl CompileMessage {
    pub fn error(message: impl Into<String>) -> Self {
        CompileMessage { level: Level::Error, message: message.into(), span: None }
    }

    pub fn with_span(
        mut self,
        span: Span
    ) -> Self {
        self.span = Some(span);
        self
    }

    pub fn display<'a>(&'a self, sources: &'a SourceMap) -> CompileErrorWithDisplay<'a> {
        CompileErrorWithDisplay { error: self, sources }
    }
}

pub struct CompileErrorWithDisplay<'a> {
    error: &'a CompileMessage,
    sources: &'a SourceMap,
}

impl Display for CompileErrorWithDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}: {}", self.error.level, self.error.message)?;
        if let Some(span) = self.error.span {
            let LineInfo { source, text, line_index, span_start, span_end } = self.sources.get_span_line(span);
            let line_number =  format!("{: >3}", line_index + 1);
            writeln!(f, " {}--> {}", " ".repeat(line_number.len()), source.name())?;
            writeln!(f, " {} | {}", line_number, text)?;
            writeln!(f, " {}   {}{}",
                " ".repeat(line_number.len()),
                " ".repeat(span_start),
                "^".repeat(span_end - span_start),
            )?;
        }
        Ok(())
    }
}
