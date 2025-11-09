use std::convert::Infallible;
use std::num::NonZero;
use std::ops::Range;
use std::sync::{Arc, OnceLock};

use camino::{Utf8Path, Utf8PathBuf};
use memchr::memchr_iter;

use crate::span::{Span, SpanMap};

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct SourceId(pub(crate) NonZero<u32>);

#[derive(Copy, Clone)]
pub struct Symbol(Span);

struct SourceInner {
    absolute_path: Option<Utf8PathBuf>,
    text: String,
    name: String,

    line_offsets: OnceLock<Vec<usize>>,
}

impl SourceInner {
    fn new(absolute_path: Option<Utf8PathBuf>, name: String, text: String) -> Self {
        SourceInner { absolute_path, name, text, line_offsets: OnceLock::new() }
    }

    fn absolute_path(&self) -> Option<&Utf8Path> {
        self.absolute_path.as_ref().map(Utf8PathBuf::as_path)
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn text(&self) -> &str {
        &self.text
    }

    /// Gets information about the surrounding line of the provided range.
    ///
    /// If the range spans multiple lines, returns `None`. Note that a trailing
    /// newline counts as spanning multiple lines.
    ///
    /// # Panics
    /// Panics if `range` is out of bounds for [`text`].
    ///
    /// [`text`]: Self::text
    fn get_line(&self, range: Range<usize>) -> Option<LineInfo> {
        assert!(range.start <= range.end && range.end <= self.text.len());

        let line_offsets = self
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
        if range.end <= line_end {
            Some(LineInfo {
                text: &self.text[line_start..line_end],
                line_index,
                span_start: range.start - line_start,
                span_end: range.end - line_start,
            })
        } else {
            None
        }
    }
}

/// Contains information about the full line that contains a range of text.
#[derive(Copy, Clone)]
pub struct LineInfo<'s> {
    /// The full line, not including the newline at the end.
    pub text: &'s str,
    /// 0-indexed count of which line this is from the start of the source.
    pub line_index: usize,
    /// The byte offset of the start of the range in [`text`].
    pub span_start: usize,
    /// The byte offset of the end of the range in [`text`].
    pub span_end: usize,
}

pub struct SourceMap(Arc<SourceMapInner>);

impl SourceMap {
    pub fn new() -> SourceMap {
        SourceMap(Arc::new(SourceMapInner {
            spans: SpanMap::new(),
            interner: dashmap::DashMap::new(),
            sources: boxcar::Vec::new(),
        }))
    }

    pub fn interner(&self) -> Interner {
        Interner { cache: quick_cache::unsync::Cache::new(1024), shared: Arc::clone(&self.0) }
    }

    pub fn add_file(&self, path: Utf8PathBuf, name: String, text: String) -> SourceId {
        self.0.add_file(path, name, text)
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

struct SourceMapInner {
    spans: SpanMap,
    interner: dashmap::DashMap<&'static str, Symbol>,
    sources: boxcar::Vec<SourceInner>,
}

impl SourceMapInner {
    fn add_file(&self, path: Utf8PathBuf, name: String, text: String) -> SourceId {
        let index = self.sources.push(SourceInner::new(Some(path), name, text));
        SourceId(NonZero::new(index as u32 + 1).expect("The number of source files is limited to u32::MAX"))
    }

    fn get_source(&self, source_id: SourceId) -> &SourceInner {
        self.sources
            .get(source_id.0.get() as usize - 1)
            .expect("The provided SourceId did not refer to a Source in this SourceMap")
    }

    /// Return a reference to a Source, without checking that it is present.
    ///
    /// # Safety
    /// `source_id` must have been created by this SourceMap.
    unsafe fn get_source_unchecked(&self, source_id: SourceId) -> &SourceInner {
        unsafe { self.sources.get_unchecked(source_id.0.get() as usize - 1) }
    }

    fn get_span(&self, span: Span) -> &str {
        self.try_get_span(span).expect("The provided Span is invalid for this SourceMap")
    }

    unsafe fn get_span_unchecked(&self, span: Span) -> &str {
        let span_info = unsafe { self.spans.resolve_unchecked(span) };
        let index = span_info.source.0.get() as usize - 1;
        let source = unsafe { self.sources.get_unchecked(index) };
        unsafe { source.text.get_unchecked(span_info.start..span_info.end) }
    }

    /// Return the slice corresponding to the span.
    ///
    /// This method returns `None` if `span` is out-of-bounds for this [`SourceMap`].
    /// In general, using [`Span`]s with a different [`SourceMap`] than they came
    /// from is safe but yields unspecified results.
    fn try_get_span(&self, span: Span) -> Option<&str> {
        let span_info = self.spans.resolve(span)?;
        let index = span_info.source.0.get() as usize - 1;
        let source = self.sources.get(index)?;
        source.text.get(span_info.start..span_info.end)
    }

    fn get_or_intern(&self, span: Span) -> Symbol {
        let text: &'static str = unsafe { transmute_lifetime(self.get_span(span)) };
        *self.interner.entry(text).or_insert(Symbol(span))
    }

    fn try_get_span_line(&self, span: Span) -> Option<LineInfo> {
        let span_info = self.spans.resolve(span)?;
        let source = self.sources.get(span_info.source.0.get() as usize)?;
        source.get_line(span_info.start..span_info.end)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct StaticString(&'static str);

pub struct Interner {
    cache: quick_cache::unsync::Cache<StaticString, Symbol>,
    shared: Arc<SourceMapInner>,
}

impl Interner {
    pub fn get_or_intern(&mut self, span: Span) -> Symbol {
        let text: &'static str = unsafe { transmute_lifetime(self.shared.get_span(span)) };
        *self
            .cache
            .get_or_insert_with(&StaticString(text), || -> Result<Symbol, Infallible> {
                Ok(self.shared.get_or_intern(span))
            })
            .unwrap()
            .unwrap()
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.shared.get_span(symbol.0)
    }
}

unsafe fn transmute_lifetime<'a, T: ?Sized>(value: &T) -> &'a T {
    unsafe { std::mem::transmute(value) }
}
