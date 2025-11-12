use std::num::NonZero;
use std::ops::{Range, RangeBounds};
use std::sync::{Arc, Once, OnceLock};

use camino::{Utf8Path, Utf8PathBuf};
use memchr::memchr_iter;

use crate::span::{Span, SpanInfo, SpanMap};

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct SourceId(pub(crate) NonZero<u32>);

pub struct Source {
    id: SourceId,
    spans: Arc<SpanMap>,

    absolute_path: Option<Utf8PathBuf>,
    text: String,
    name: String,

    line_offsets: OnceLock<Vec<usize>>,
}

impl Source {
    pub fn id(&self) -> SourceId {
        self.id
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn span(&self, range: Range<usize>) -> Span {
        self.spans.add(SpanInfo {
            source: self.id,
            start: range.start,
            end: range.end
        })
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
    pub fn get_line(&self, range: Range<usize>) -> Option<LineInfo<'_>> {
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

pub struct SourceMap(pub(crate) Arc<SourceMapInner>);

impl SourceMap {
    pub fn new() -> SourceMap {
        SourceMap(Arc::new(SourceMapInner {
            spans: Arc::new(SpanMap::new()),
            sources: boxcar::Vec::new(),
        }))
    }

    pub fn add_file(&self, path: Utf8PathBuf, name: String, text: String) -> SourceId {
        self.0.add_file(path, name, text)
    }

    pub fn get_source(&self, source_id: SourceId) -> &Source {
        self.0.get_source(source_id)
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) struct SourceMapInner {
    spans: Arc<SpanMap>,
    sources: boxcar::Vec<Source>,
}

impl SourceMapInner {
    pub fn add_file(&self, path: Utf8PathBuf, name: String, text: String) -> SourceId {
        let index = self.sources.push_with(move |index| {
            let id = SourceId(NonZero::new(index as u32 + 1).expect("The number of source files is limited to u32::MAX - 1"));
            Source {
                id,
                spans: Arc::clone(&self.spans),
                absolute_path: Some(path),
                text,
                name,
                line_offsets: OnceLock::new(),
            }
        });
        SourceId(NonZero::new(index as u32 + 1).unwrap())
    }

    pub fn get_source(&self, source_id: SourceId) -> &Source {
        self.sources
            .get(source_id.0.get() as usize - 1)
            .expect("The provided SourceId did not refer to a Source in this SourceMap")
    }

    /// Return a reference to a Source, without checking that it is present.
    ///
    /// # Safety
    /// `source_id` must have been created by this SourceMap.
    pub unsafe fn get_source_unchecked(&self, source_id: SourceId) -> &Source {
        unsafe { self.sources.get_unchecked(source_id.0.get() as usize - 1) }
    }

    pub fn get_span(&self, span: Span) -> &str {
        self.try_get_span(span).expect("The provided Span is invalid for this SourceMap")
    }

    pub unsafe fn get_span_unchecked(&self, span: Span) -> &str {
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
    pub fn try_get_span(&self, span: Span) -> Option<&str> {
        let span_info = self.spans.resolve(span)?;
        let index = span_info.source.0.get() as usize - 1;
        let source = self.sources.get(index)?;
        source.text.get(span_info.start..span_info.end)
    }

    pub fn try_get_span_line(&self, span: Span) -> Option<LineInfo> {
        let span_info = self.spans.resolve(span)?;
        let source = self.sources.get(span_info.source.0.get() as usize)?;
        source.get_line(span_info.start..span_info.end)
    }
}
