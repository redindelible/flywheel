use std::fmt::{Debug, Formatter};
use std::hash::BuildHasher;
use std::num::NonZero;
use std::sync::RwLock;

use rustc_hash::FxBuildHasher;

use crate::source::SourceId;

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Eq, PartialEq)]
struct u48([u8; 6]);

const _: () = const {
    assert!(size_of::<u48>() == 6);
    assert!(align_of::<u48>() == 1);
};

impl u48 {
    pub const MAX: u64 = (1u64 << 48) - 1;

    fn to_u64(self) -> u64 {
        let [a, b, c, d, e, f] = self.0;
        u64::from_le_bytes([a, b, c, d, e, f, 0, 0])
    }
}

impl TryFrom<u64> for u48 {
    type Error = u64;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        let [a, b, c, d, e, f, i, j] = value.to_le_bytes();
        if i == 0 && j == 0 { Ok(u48([a, b, c, d, e, f])) } else { Err(value) }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(align(8))]
pub struct Span(SpanInner);

const _: () = const {
    assert!(size_of::<Span>() == 8);
    assert!(align_of::<Span>() == 8);
};

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            SpanInner::Inline { source, offset, length } => f
                .debug_struct("Span")
                .field("source", &source.get())
                .field("offset", &offset)
                .field("length", &length)
                .finish(),
            SpanInner::Interned(interned) => f.debug_struct("Span").field("intern", &interned.to_u64()).finish(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum SpanInner {
    Inline { source: NonZero<u16>, offset: u32, length: u16 },
    Interned(u48),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SpanInfo {
    pub source: SourceId,
    pub start: usize,
    pub end: usize,
}

pub(crate) struct SpanMap {
    deduplicator: RwLock<hashbrown::HashTable<usize>>,
    spans: boxcar::Vec<SpanInfo>,
}

impl SpanMap {
    pub fn new() -> SpanMap {
        SpanMap { deduplicator: RwLock::default(), spans: boxcar::Vec::new() }
    }

    pub fn add(&self, span_info: SpanInfo) -> Span {
        let maybe_source = NonZero::<u16>::try_from(span_info.source.0).ok();
        let maybe_offset = u32::try_from(span_info.start).ok();
        let maybe_length = u16::try_from(span_info.end.wrapping_sub(span_info.start)).ok();
        if let (Some(source), Some(offset), Some(length)) = (maybe_source, maybe_offset, maybe_length) {
            Span(SpanInner::Inline { source, offset, length })
        } else {
            let mut deduplicator = self.deduplicator.write().unwrap();

            let hash = FxBuildHasher.hash_one(span_info);
            let entry = deduplicator.entry(
                hash,
                |&index| self.spans[index] == span_info,
                |&index| FxBuildHasher.hash_one(self.spans[index]),
            );
            let actual_index = *entry
                .or_insert_with(|| {
                    self.spans.push_with(|index| {
                        if index as u64 > u48::MAX {
                            panic!("The number of spans is limited to 2**48");
                        }
                        span_info
                    })
                })
                .get();

            Span(SpanInner::Interned(u48::try_from(actual_index as u64).unwrap()))
        }
    }

    pub fn resolve(&self, span: Span) -> Option<SpanInfo> {
        match span.0 {
            SpanInner::Inline { source, offset, length } => Some(SpanInfo {
                source: SourceId(source.into()),
                start: offset as usize,
                end: offset as usize + length as usize,
            }),
            SpanInner::Interned(index) => self.spans.get(index.to_u64() as usize).copied(),
        }
    }

    pub unsafe fn resolve_unchecked(&self, span: Span) -> SpanInfo {
        match span.0 {
            SpanInner::Inline { source, offset, length } => SpanInfo {
                source: SourceId(source.into()),
                start: offset as usize,
                end: offset as usize + length as usize,
            },
            SpanInner::Interned(index) => unsafe { *self.spans.get_unchecked(index.to_u64() as usize) },
        }
    }
}

#[cfg(test)]
mod test {
    use std::num::NonZero;

    use crate::Span;
    use crate::source::SourceId;
    use crate::span::{SpanInfo, SpanInner, SpanMap, u48};

    #[test]
    fn test_u48_round_trip() {
        for value in [0, 1, 0xFF, 0xabcd, 0x4567_89ab_cdef] {
            let packed = u48::try_from(value).expect("Should fit into 48 bits");
            assert_eq!(packed.to_u64(), value);
        }
    }

    #[test]
    fn test_u48_too_big() {
        for value in [0x0001_0000_0000_0000, u64::MAX] {
            assert!(u48::try_from(value).is_err(), "Should be too big");
        }
    }

    #[test]
    fn test_interner_inline() {
        let spans = SpanMap::new();
        let span_info = SpanInfo { source: SourceId(NonZero::new(1).unwrap()), start: 16, end: 18 };
        let span = spans.add(span_info);
        assert!(matches!(span.0, SpanInner::Inline { .. }));
        assert_eq!(Some(span_info), spans.resolve(span));
    }

    #[test]
    fn test_interner_interned() {
        let spans = SpanMap::new();

        let infos = [
            SpanInfo { source: SourceId(NonZero::new(u32::MAX - 1).unwrap()), start: 16, end: 18 },
            SpanInfo { source: SourceId(NonZero::new(1).unwrap()), start: 0x0001_0000_0000, end: 0x0001_0000_0015 },
            SpanInfo { source: SourceId(NonZero::new(1).unwrap()), start: 15, end: 0x0001_0015 },
        ];
        for span_info in infos {
            let span = spans.add(span_info);
            assert!(matches!(span.0, SpanInner::Interned(_)));
            let recovered_span_info = spans.resolve(span);
            assert_eq!(Some(span_info), recovered_span_info);
        }
    }

    #[test]
    fn test_equal_span() {
        let spans = SpanMap::new();

        let infos = [
            SpanInfo { source: SourceId(NonZero::new(1).unwrap()), start: 16, end: 18 },
            SpanInfo { source: SourceId(NonZero::new(u32::MAX - 1).unwrap()), start: 16, end: 18 },
            SpanInfo { source: SourceId(NonZero::new(1).unwrap()), start: 0x0001_0000_0000, end: 0x0001_0000_0015 },
            SpanInfo { source: SourceId(NonZero::new(1).unwrap()), start: 15, end: 0x0001_0015 },
        ];
        let first_spans: Vec<Span> = infos.iter().map(|&s| spans.add(s)).collect();

        for (span_info, first_span) in infos.into_iter().zip(first_spans) {
            assert_eq!(first_span, spans.add(span_info));
            assert_eq!(spans.resolve(first_span), Some(span_info));
        }
    }
}
