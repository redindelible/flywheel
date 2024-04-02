use std::collections::HashMap;
use std::fmt::Debug;
use std::{iter, mem};
use std::marker::PhantomData;
use std::ops::Range;
// use std::rc::Rc;
use crate::keymap::{declare_key_type, KeyMap};

declare_key_type! {
    pub struct BlockKey;
}

pub trait Asm {
    type Writer<'a> where Self: 'a;
    type Fixup;

    fn new_writer(block: &mut Block<Self>) -> Self::Writer<'_>;

    fn fix_pessimistic_size(data: &Self::Fixup) -> u8;
    fn fix_size_estimate(offset_estimate: isize, data: &Self::Fixup) -> u8;
    fn fix<'m>(raw_offset: isize, size: u8, data: &Self::Fixup, mem: &mut &'m mut [u8]);
}

struct Fixup<F> {
    block_offset: usize,
    block: BlockKey,
    data: F,
    enforce_size: u8
}

pub struct Block<A: Asm + ?Sized> {
    machine_code: Vec<u8>,
    pessimistic_size: usize,
    fixups: Vec<Fixup<A::Fixup>>,
    ranges: Vec<Range<usize>>,
    recent: Range<usize>
}

impl<A: Asm> Block<A> {
    fn new() -> Self {
        Block { machine_code: Vec::new(), pessimistic_size: 0, fixups: Vec::new(), ranges: Vec::new(), recent: 0..0 }
    }

    // fn align(&self) -> usize { 1 }
    fn size(&self) -> usize {
        self.pessimistic_size
    }

    pub fn write<const N: usize>(&mut self, vals: [u8; N]) {
        self.machine_code.extend_from_slice(&vals);
        self.recent.end += vals.len();
        self.pessimistic_size += vals.len();
    }

    pub fn write_fixup(&mut self, block: BlockKey, data: A::Fixup) {
        self.pessimistic_size += A::fix_pessimistic_size(&data) as usize;
        let new = self.recent.end..self.recent.end;
        self.ranges.push(mem::replace(&mut self.recent, new));
        let block_offset = self.machine_code.len();
        self.fixups.push(Fixup { block_offset, block, data, enforce_size: 0 });
    }
}

pub struct CodeBuilder<A> where A: Asm {
    blocks: KeyMap<BlockKey, Block<A>>,
    _phantom: PhantomData<A>
}

impl<A> CodeBuilder<A> where A: Asm {
    pub fn new() -> CodeBuilder<A> {
        CodeBuilder { blocks: KeyMap::new(), _phantom: PhantomData }
    }

    pub fn add_block(&mut self) -> BlockKey {
        self.blocks.reserve()
    }

    pub fn build<O>(&mut self, block: BlockKey, f: impl for<'a> FnOnce(&mut A::Writer<'a>) -> O) -> O {
        if !self.blocks.is_initialized(block) {
            self.blocks.insert(block, Block::new());
        }

        let mut writer = A::new_writer(&mut self.blocks[block]);
        f(&mut writer)
    }

    pub fn finish<M>(mut self, alloc_mem: impl FnOnce(usize) -> M, get_mut: impl FnOnce(&mut M) -> &mut [u8]) -> M {
        use std::io::Write;

        let mut block_positions_estimate: HashMap<BlockKey, usize> = HashMap::new();
        let mut total_size_estimate: usize = 0;
        for (block_key, block) in self.blocks.iter() {
            // total_size = total_size.next_multiple_of(block.align());
            block_positions_estimate.insert(block_key, total_size_estimate);
            total_size_estimate += block.size();
        }

        let mut block_positions: HashMap<BlockKey, usize> = HashMap::new();
        let mut actual_size: usize = 0;
        for (block_key, block) in self.blocks.iter_mut() {
            let start = block_positions_estimate[&block_key];
            let mut saved = 0;
            for fixup in &mut block.fixups {
                let fixup_start = start + fixup.block_offset;
                let fixup_offset = block_positions_estimate[&fixup.block] as i64 - fixup_start as i64;

                let enforced_size = A::fix_size_estimate(fixup_offset as isize, &fixup.data);
                saved += A::fix_pessimistic_size(&fixup.data) - enforced_size;
                fixup.enforce_size = enforced_size;
            }
            block_positions.insert(block_key, actual_size);
            actual_size += block.size() - saved as usize;
        }
        debug_assert!(actual_size <= total_size_estimate);

        let mut mem_holder = alloc_mem(actual_size);
        let mut mem = get_mut(&mut mem_holder);
        let mut offset = 0;
        debug_assert!(mem.len() >= actual_size);

        for block in self.blocks.values() {
            for (range, fixup) in block.ranges.iter().zip(&block.fixups) {
                offset += range.len();
                let _ = mem.write(&block.machine_code[range.clone()]);
                A::fix(block_positions[&fixup.block] as isize - offset as isize, fixup.enforce_size, &fixup.data, &mut mem);
                offset += fixup.enforce_size as usize;
            }
            let _ = mem.write(&block.machine_code[block.recent.clone()]);
            offset += block.recent.len();
        }
        debug_assert_eq!(offset, actual_size);

        mem_holder
    }
}
