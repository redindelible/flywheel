use std::num::NonZero;
use std::ops::Range;
use std::sync::{Arc, OnceLock};

use camino::Utf8PathBuf;
use memchr::memchr_iter;

use crate::Symbol;
use crate::span::{Span, SpanInfo, SpanMap};

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct SourceId(pub(crate) NonZero<u32>);

/// A `Source` can be an actual file, an input string, or even a virtual text
/// for builtins to refer to. This type cannot be directly constructed and
/// must instead be obtained from a [`SourceMap`].
pub struct Source {
    /// The SourceId of this `Source` within its [`SourceMap`].
    id: SourceId,
    /// Used to add [`Span`]s into this `Source`. All `Source`s within a
    /// [`SourceMap`] share one underlying [`SpanMap`].
    spans: Arc<SpanMap>,

    /// If this `Source` is from an actual file, the path to that file.
    _absolute_path: Option<Utf8PathBuf>,
    /// The text of this `Source`. This _must_ not change after this `Source`
    /// is constructed, as a number of methods rely on this have a stable deref
    /// for safety.
    text: String,
    /// The human-readable name of this `Source`.
    name: String,

    /// Stores the byte offsets of each newline in the text, appended with the
    /// length of the text for convenience. Computed on demand and then cached.
    line_offsets: OnceLock<Vec<usize>>,
}

impl Source {
    /// The SourceId of this `Source` within the containing [`SourceMap`].
    pub fn id(&self) -> SourceId {
        self.id
    }

    /// Get the full text of this `Source`. This reference is guaranteed to be
    /// alive for the life of the containing [`SourceMap`].
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Get the human-readable name of this `Source`.
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn add_span(&self, range: Range<usize>) -> Span {
        assert!(range.start <= range.end && range.end <= self.text.len());
        self.spans.add(SpanInfo { source: self.id, start: range.start, end: range.end })
    }

    pub fn add_eof_span(&self) -> Span {
        self.add_span(self.text.len()..self.text.len())
    }

    /// Gets information about the surrounding line of the provided range.
    ///
    /// # Panics
    /// Panics if `range` is out of bounds this `Source`'s text.
    fn get_line(&self, range: Range<usize>) -> LineInfo<'_> {
        assert!(range.start <= range.end && range.end <= self.text.len());

        let line_offsets: &[usize] = self
            .line_offsets
            .get_or_init(|| {
                let mut line_offsets: Vec<usize> = memchr_iter(b'\n', self.text.as_bytes()).collect();
                line_offsets.push(self.text.len());
                line_offsets
            })
            .as_slice();
        let line_index = line_offsets.binary_search(&range.start).unwrap_or_else(|n| n);
        let (line_start, line_end) = match line_index {
            0 => (0, line_offsets[0]),
            line_offsets_index => (line_offsets[line_offsets_index - 1] + 1, line_offsets[line_offsets_index]),
        };

        LineInfo {
            source: self,
            text: &self.text[line_start..line_end],
            line_index,
            span_start: range.start - line_start,
            span_end: range.end - line_start,
        }
    }
}

/// Contains information about the full line that contains a range of text.
#[derive(Copy, Clone)]
pub struct LineInfo<'s> {
    /// The source containing this `Span`.
    pub source: &'s Source,
    /// The full text of the line, not including the trailing newline.
    pub text: &'s str,
    /// 0-indexed count of which line this is.
    pub line_index: usize,
    /// The byte offset of the start of the `Span` within [`text`].
    pub span_start: usize,
    /// The byte offset of the end of the `Span` within [`text`].
    pub span_end: usize,
}

/// This type keeps track of every [`Source`] created with it, which allows
/// for it to resolve any [`Span`] regardless of which specific `Source` it
/// came from.
pub struct SourceMap {
    /// Interns less common [`Span`]s, allowing for the size of the `Span`
    /// type itself to remain small.
    spans: Arc<SpanMap>,
    /// A concurrent, append-only list of every [`Source`].
    sources: boxcar::Vec<Source>,
}

impl SourceMap {
    /// Creates a new `SourceMap`.
    pub fn new() -> SourceMap {
        let spans = Arc::new(SpanMap::new());
        let dummy_source = Source {
            id: SourceId(NonZero::<u32>::MAX),
            spans: Arc::clone(&spans),
            _absolute_path: None,
            text: "".into(),
            name: "<dummy>".into(),
            line_offsets: OnceLock::new(),
        };
        SourceMap { spans, sources: boxcar::Vec::from_iter([dummy_source]) }
    }

    fn add_source(&self, name: String, text: String, path: Option<Utf8PathBuf>) -> &Source {
        let index = self.sources.push_with(move |index| {
            let id = SourceId(NonZero::new(index as u32)
                .expect("The number of source files is limited to u32::MAX - 1"));
            Source {
                id,
                spans: Arc::clone(&self.spans),
                text,
                name,
                _absolute_path: path,
                line_offsets: OnceLock::new(),
            }
        });
        self.sources.get(index).unwrap()
    }

    pub fn add_builtins(&self, text: String) -> &Source {
        self.add_source("<builtins>".into(), text, None)
    }

    pub fn add_file(&self, path: impl Into<Utf8PathBuf>, text: String) -> &Source {
        let path = path.into();
        self.add_source(path.clone().into_string(), text, Some(path))
    }

    pub fn add_string(&self, name: impl Into<String>, text: String) -> &Source {
        self.add_source(name.into(), text, None)
    }

    pub fn get_source(&self, source_id: SourceId) -> &Source {
        self.sources
            .get(source_id.0.get() as usize)
            .expect("The provided SourceId did not refer to a Source in this SourceMap")
    }

    /// Return a reference to a Source, without checking that it is present.
    ///
    /// # Safety
    /// `source_id` must have been created by this SourceMap.
    pub unsafe fn get_source_unchecked(&self, source_id: SourceId) -> &Source {
        unsafe { self.sources.get_unchecked(source_id.0.get() as usize) }
    }

    pub fn get_span_info(&self, span: Span) -> SpanInfo {
        self.spans.resolve(span).expect("The provided Span is invalid for this SourceMap")
    }

    /// Return the slice corresponding to the span. If the span is not
    /// originally from this `SourceMap`, then this method may either panic
    /// or return an arbitrary string.
    ///
    /// # Panics
    /// May panic if `span` is not from this `SourceMap`.
    pub fn get_span(&self, span: Span) -> &str {
        self.try_get_span(span).expect("The provided Span is invalid for this SourceMap")
    }

    /// Return the slice corresponding to the symbol. If the symbol is not
    /// originally from this `SourceMap`, then this method may either panic
    /// or return an arbitrary string.
    ///
    /// # Panics
    /// May panic if `symbol` is not from this `SourceMap`.
    pub fn get_symbol(&self, symbol: Symbol) -> &str {
        self.try_get_span(symbol.span()).expect("The provided Span is invalid for this SourceMap")
    }

    pub fn get_span_line(&self, span: Span) -> LineInfo<'_> {
        // todo this error message lies, it could also be None of the span spans multiple lines
        self.try_get_span_line(span).expect("The provided Span is invalid for this SourceMap")
    }

    pub unsafe fn get_span_unchecked(&self, span: Span) -> &str {
        let span_info = unsafe { self.spans.resolve_unchecked(span) };
        let index = span_info.source.0.get() as usize;
        let source = unsafe { self.sources.get_unchecked(index) };
        unsafe { source.text.get_unchecked(span_info.start..span_info.end) }
    }

    /// Return the slice corresponding to the span. If the span is not
    /// originally from this `SourceMap`, then this method may either return
    /// `None` or an arbitrary string.
    fn try_get_span(&self, span: Span) -> Option<&str> {
        let span_info = self.spans.resolve(span)?;
        let index = span_info.source.0.get() as usize;
        let source = self.sources.get(index)?;
        source.text.get(span_info.start..span_info.end)
    }

    fn try_get_span_line(&self, span: Span) -> Option<LineInfo<'_>> {
        let span_info = self.spans.resolve(span)?;
        let index = span_info.source.0.get() as usize;
        let source = self.sources.get(index)?;
        Some(source.get_line(span_info.start..span_info.end))
    }
}

#[cfg(test)]
mod test {
    use std::num::NonZero;
    use std::sync::Arc;

    use crate::interner::Interner;
    use crate::source::{SourceId, SourceMap};

    // --- SourceMap construction ---

    #[test]
    fn test_add_source_variants() {
        let sources = SourceMap::new();
        let s = sources.add_string("my source", "text".to_string());
        assert_eq!(s.name(), "my source");
        assert_eq!(s.text(), "text");
        let s = sources.add_builtins("text".to_string());
        assert_eq!(s.name(), "<builtins>");
        assert_eq!(s.text(), "text");
        let s = sources.add_file("path/to/file.fw", "text".to_string());
        assert_eq!(s.name(), "path/to/file.fw");
        assert_eq!(s.text(), "text");
    }

    #[test]
    fn test_source_ids_are_distinct() {
        let sources = SourceMap::new();
        let a = sources.add_string("a", String::new());
        let b = sources.add_string("b", String::new());
        let c = sources.add_string("c", String::new());
        assert_ne!(a.id(), b.id());
        assert_ne!(b.id(), c.id());
        assert_ne!(a.id(), c.id());
    }

    #[test]
    fn test_get_symbol() {
        let sources = Arc::new(SourceMap::new());
        let source = sources.add_string("test", "hello".to_string());
        let mut interner = Interner::new(Arc::clone(&sources));
        let sym = interner.get_or_intern(source.add_span(0..5));
        assert_eq!(sources.get_symbol(sym), "hello");
    }

    #[test]
    fn test_get_source_round_trip() {
        let sources = SourceMap::new();
        let source = sources.add_string("test", "content".to_string());
        let retrieved = sources.get_source(source.id());
        assert_eq!(retrieved.text(), "content");
        assert_eq!(retrieved.name(), "test");
    }

    #[test]
    #[should_panic]
    fn test_get_source_panics_on_invalid_id() {
        let sources = SourceMap::new();
        sources.get_source(SourceId(NonZero::new(999).unwrap()));
    }

    // --- add_span / add_eof_span ---

    #[test]
    fn test_span_resolution() {
        let sources = SourceMap::new();
        let source = sources.add_string("test", "hello world".to_string());
        assert_eq!(sources.get_span(source.add_span(0..5)), "hello");
        assert_eq!(sources.get_span(source.add_span(6..11)), "world");
        assert_eq!(sources.get_span(source.add_span(5..5)), ""); // zero-length mid-text
        assert_eq!(sources.get_span(source.add_eof_span()), "");  // eof span
        let info = sources.get_span_info(source.add_span(2..7));
        assert_eq!(info.source, source.id());
        assert_eq!(info.start, 2);
        assert_eq!(info.end, 7);
    }

    #[test]
    #[should_panic]
    fn test_add_span_panics_when_end_past_text() {
        let sources = SourceMap::new();
        sources.add_string("test", "hello".to_string()).add_span(0..6);
    }

    #[test]
    #[should_panic]
    fn test_add_span_panics_when_start_after_end() {
        let sources = SourceMap::new();
        sources.add_string("test", "hello".to_string()).add_span(3..2);
    }

    // --- get_span_line ---

    #[test]
    fn test_get_line_multiline() {
        let sources = SourceMap::new();

        // Single-line source: entire text is line 0.
        let source = sources.add_string("test", "hello world".to_string());
        let line = sources.get_span_line(source.add_span(6..11));
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (0, "hello world", 6, 11));

        // Multi-line: first, middle, and last line.
        let source = sources.add_string("test", "foo\nbar\nbaz".to_string());
        let line = sources.get_span_line(source.add_span(1..2));
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (0, "foo", 1, 2));
        let line = sources.get_span_line(source.add_span(4..7));
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (1, "bar", 0, 3));
        let line = sources.get_span_line(source.add_span(9..10));
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (2, "baz", 1, 2));
    }

    #[test]
    fn test_get_line_newline_spans() {
        // Both cases use "foo\nbar" and exercise spans where span_end extends past
        // the line's own text. get_line keys off range.start for the line lookup.
        let sources = SourceMap::new();
        let source = sources.add_string("test", "foo\nbar".to_string());

        // Span crossing the newline: keys off start=1 → line 0, span_end past line text.
        let line = sources.get_span_line(source.add_span(1..6));
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (0, "foo", 1, 6));

        // Span pointing to the newline itself: binary_search hits the newline's offset
        // in line_offsets, landing on the line that ends with it.
        let line = sources.get_span_line(source.add_span(3..4));
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (0, "foo", 3, 4));
    }

    #[test]
    fn test_get_line_eof_spans() {
        let sources = SourceMap::new();

        // Empty source: single line with empty text.
        let source = sources.add_string("test", "".to_string());
        let line = sources.get_span_line(source.add_eof_span());
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (0, "", 0, 0));

        // Trailing newline: EOF lands on the empty line after it.
        let source = sources.add_string("test", "foo\n".to_string());
        let line = sources.get_span_line(source.add_eof_span());
        assert_eq!((line.line_index, line.text, line.span_start, line.span_end), (1, "", 0, 0));
    }

    #[test]
    fn test_multiple_sources_route_correctly() {
        // Spans from two different sources must resolve to the right source's text,
        // both for get_span and get_span_line. This would fail if get_span or
        // get_span_line always used the first source's index.
        let sources = SourceMap::new();
        let a = sources.add_string("a", "foo\nbar".to_string());
        let b = sources.add_string("b", "baz\nqux".to_string());

        assert_eq!(sources.get_span(a.add_span(0..3)), "foo");
        assert_eq!(sources.get_span(b.add_span(0..3)), "baz");

        let line_a = sources.get_span_line(a.add_span(4..7));
        assert_eq!(line_a.source.id(), a.id());
        assert_eq!(line_a.text, "bar");

        let line_b = sources.get_span_line(b.add_span(4..7));
        assert_eq!(line_b.source.id(), b.id());
        assert_eq!(line_b.text, "qux");
    }

    #[test]
    fn test_get_line_leading_newline() {
        // Text starting with '\n' means line 0 is empty. The first real content
        // is on line 1. Exercises the 0 => arm with an empty line at the front.
        let sources = SourceMap::new();
        let source = sources.add_string("test", "\nfoo".to_string());

        let line = sources.get_span_line(source.add_span(0..0));
        assert_eq!(line.line_index, 0);
        assert_eq!(line.text, "");
        assert_eq!(line.span_start, 0);
        assert_eq!(line.span_end, 0);

        let line = sources.get_span_line(source.add_span(1..4));
        assert_eq!(line.line_index, 1);
        assert_eq!(line.text, "foo");
        assert_eq!(line.span_start, 0);
        assert_eq!(line.span_end, 3);
    }

    // --- unsafe accessors ---

    #[test]
    fn test_unsafe_accessors_match_safe() {
        let sources = SourceMap::new();
        let source = sources.add_string("test", "hello world".to_string());
        let span = source.add_span(6..11);
        unsafe {
            let u = sources.get_source_unchecked(source.id());
            assert_eq!(u.text(), source.text());
            assert_eq!(u.name(), source.name());
            assert_eq!(sources.get_span_unchecked(span), sources.get_span(span));
        }
    }
}
