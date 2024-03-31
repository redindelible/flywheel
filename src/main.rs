mod keymap;

use crate::keymap::{declare_key_type, KeyMap};

declare_key_type! {
    struct BlockKey;
}


struct Block {
    machine_code: Vec<u8>
}

impl Block {
    fn new() -> Block {
        Block { machine_code: Vec::new() }
    }

    fn size(&self) -> usize {
        self.machine_code.len()
    }
}

struct BlockBuilder<'a> {
    code: &'a mut Block
}

impl<'a> BlockBuilder<'a> {
    fn write<const N: usize>(&mut self, vals: [u8; N]) {
        self.code.machine_code.extend_from_slice(&vals);
    }
}

struct CodeBuilder {
    blocks: KeyMap<BlockKey, Block>
}

impl CodeBuilder {
    fn new() -> CodeBuilder {
        CodeBuilder { blocks: KeyMap::new() }
    }

    fn add_block(&mut self) -> BlockKey {
        self.blocks.reserve()
    }

    fn build<O>(&mut self, block: BlockKey, f: impl FnOnce(BlockBuilder) -> O) -> O {
        if !self.blocks.is_initialized(block) {
            self.blocks.insert(block, Block::new());
        }
        let builder = BlockBuilder { code: &mut self.blocks[block] };
        f(builder)
    }

    fn finish<M>(mut self, alloc_mem: impl FnOnce(usize) -> M, get_mut: impl FnOnce(&mut M) -> &mut [u8]) -> M {
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


fn main() {
    let mut code = CodeBuilder::new();
    let entry = code.add_block();

    code.build(entry, |mut builder| {
        builder.write([0xB8, 0x04, 0x00, 0x00, 0x00]);
        builder.write([0xC3]);
    });

    let mem = code.finish(|size| memmap2::MmapMut::map_anon(size).unwrap(), |mem| mem);
    let exec = mem.make_exec().unwrap();
    let ptr = exec.as_ptr();
    let num = unsafe {
        let f: unsafe extern "C" fn () -> i32 = std::mem::transmute(ptr);
        f()
    };

    println!("{:?}", num);
}
