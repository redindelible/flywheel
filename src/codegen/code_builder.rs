use crate::keymap::{declare_key_type, KeyMap};

declare_key_type! {
    pub struct BlockKey;
}

pub struct Block {
    machine_code: Vec<u8>
}

impl Block {
    fn new() -> Block {
        Block { machine_code: Vec::new() }
    }

    fn size(&self) -> usize {
        self.machine_code.len()
    }

    pub fn write<const N: usize>(&mut self, vals: [u8; N]) {
        self.machine_code.extend_from_slice(&vals);
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

    pub fn finish<M>(mut self, alloc_mem: impl FnOnce(usize) -> M, get_mut: impl FnOnce(&mut M) -> &mut [u8]) -> M {
        use std::io::Write;

        let total_size = self.blocks.values().map(Block::size).sum();
        let mut mem_holder = alloc_mem(total_size);
        let mut mem = get_mut(&mut mem_holder);
        assert!(mem.len() >= total_size);

        for block in self.blocks.values() {
            let _ = mem.write(&block.machine_code);
        }

        mem_holder
    }
}