use std::collections::HashMap;
use crate::keymap::{declare_key_type, KeyMap};

declare_key_type! {
    pub struct BlockKey;
}

enum RelocationSize {
    I32
}

struct Relocation {
    block_offset: usize,
    block: BlockKey,
    size: RelocationSize,
    offset: i8
}

pub struct Block {
    machine_code: Vec<u8>,
    relocations: Vec<Relocation>
}

impl Block {
    fn new() -> Block {
        Block { machine_code: Vec::new(), relocations: Vec::new() }
    }

    fn align(&self) -> usize { 1 }
    fn size(&self) -> usize {
        self.machine_code.len()
    }

    pub fn write<const N: usize>(&mut self, vals: [u8; N]) {
        self.machine_code.extend_from_slice(&vals);
    }

    pub fn write_relocation_i32(&mut self, block: BlockKey, offset: i8) {
        let block_offset = self.machine_code.len();
        self.write([0; 4]);
        self.relocations.push(Relocation { block_offset, block, size: RelocationSize::I32, offset });
    }
}

pub struct CodeBuilder {
    blocks: KeyMap<BlockKey, Block>
}

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder { blocks: KeyMap::new() }
    }

    pub fn add_block(&mut self) -> BlockKey {
        self.blocks.reserve()
    }

    pub fn build<O>(&mut self, block: BlockKey, f: impl FnOnce(&mut Block) -> O) -> O {
        if !self.blocks.is_initialized(block) {
            self.blocks.insert(block, Block::new());
        }

        f(&mut self.blocks[block])
    }

    pub fn finish<M>(self, alloc_mem: impl FnOnce(usize) -> M, get_mut: impl FnOnce(&mut M) -> &mut [u8]) -> M {
        let mut block_positions: HashMap<BlockKey, usize> = HashMap::new();
        let mut total_size: usize = 0;
        for (block_key, block) in self.blocks.iter() {
            total_size = total_size.next_multiple_of(block.align());
            block_positions.insert(block_key, total_size);
            total_size += block.size();
        }

        let mut mem_holder = alloc_mem(total_size);
        let mut mem = get_mut(&mut mem_holder);
        assert!(mem.len() >= total_size);

        for (block_key, block) in self.blocks.iter() {
            let start = block_positions[&block_key];
            let write_to = &mut mem[start..start+block.size()];
            write_to.copy_from_slice(&block.machine_code);

            for reloc in &block.relocations {
                let reloc_start = start + reloc.block_offset;
                let reloc_offset = block_positions[&reloc.block] as i64 - reloc_start as i64 + reloc.offset as i64;

                match reloc.size {
                    RelocationSize::I32 => {
                        write_to[reloc.block_offset..reloc.block_offset+4].copy_from_slice(&(reloc_offset as i32).to_le_bytes());
                    }
                }
            }
        }

        mem_holder
    }
}