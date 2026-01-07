use std::backtrace::Backtrace;
use std::fmt::{Debug, Display, Formatter};

use flywheel_sources::{LineInfo, SourceMap, Span};

pub type CompileResult<T> = Result<T, Box<CompileMessage>>;

#[derive(Debug)]
pub enum Level {
    Note,
    Error,
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Level::Note => "Note",
                Level::Error => "Error",
            }
        )
    }
}

struct UseDisplay<T>(T);

impl<T: Display> Debug for UseDisplay<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
pub struct CompileMessage {
    level: Level,
    message: String,
    span: Option<Span>,
    children: Vec<CompileMessage>,
    backtrace: UseDisplay<Backtrace>,
}

impl CompileMessage {
    pub fn note(message: impl Into<String>) -> Self {
        CompileMessage {
            level: Level::Note,
            message: message.into(),
            span: None,
            children: vec![],
            backtrace: UseDisplay(Backtrace::capture()),
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        CompileMessage {
            level: Level::Error,
            message: message.into(),
            span: None,
            children: vec![],
            backtrace: UseDisplay(Backtrace::capture()),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_child(mut self, child: CompileMessage) -> Self {
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
            let line_number = format!("{: >3}", line_index + 1);
            writeln!(f, "{} {}--> {}", &indent, " ".repeat(line_number.len()), source.name())?;
            writeln!(f, "{}{} | {}", &indent, line_number, text)?;
            writeln!(
                f,
                "{}{}   {}{}",
                &indent,
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

#[cfg(test)]
mod tests {
    use flywheel_sources::SourceMap;

    use crate::*;

    #[test]
    fn test_basic_error() {
        let sources = SourceMap::new();
        let msg = CompileMessage::error("something went wrong");
        let output = format!("{}", msg.display(&sources));
        assert_eq!(output, "Error: something went wrong\n");
    }

    #[test]
    fn test_basic_note() {
        let sources = SourceMap::new();
        let msg = CompileMessage::note("here is a note");
        let output = format!("{}", msg.display(&sources));
        assert_eq!(output, "Note: here is a note\n");
    }

    #[test]
    fn test_error_with_span() {
        let sources = SourceMap::new();
        let source = sources.add_file("test.fly", "let x = 123;".to_string());
        let span = source.add_span(8..11);

        let msg = CompileMessage::error("invalid number").with_span(span);
        let output = format!("{}", msg.display(&sources));

        let expected = "Error: invalid number\n    --> test.fly\n  1 | let x = 123;\n              ^^^\n";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_nested_messages() {
        let sources = SourceMap::new();
        let source = sources.add_file("test.fly", "fn main() { }".to_string());
        let span = source.add_span(3..7); // "main"

        let child = CompileMessage::note("defined here").with_span(span);
        let parent = CompileMessage::error("duplicate function").with_child(child);

        let output = format!("{}", parent.display(&sources));

        let expected = "Error: duplicate function\n |  Note: defined here\n |      --> test.fly\n |    1 | fn main() { }\n |           ^^^^\n";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_multiple_children() {
        let sources = SourceMap::new();
        let child1 = CompileMessage::note("note 1");
        let child2 = CompileMessage::note("note 2");
        let parent = CompileMessage::error("main error").with_child(child1).with_child(child2);

        let output = format!("{}", parent.display(&sources));
        let expected = "Error: main error\n |  Note: note 1\n |  Note: note 2\n";
        assert_eq!(output, expected);
    }
}
