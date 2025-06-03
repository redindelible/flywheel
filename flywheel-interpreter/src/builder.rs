use crate::instr::Instruction;

pub struct CodeChunk {
    required_registers: u32,
    instructions: Box<[u32]>
}

impl CodeChunk {
    pub fn builder() -> CodeChunkBuilder {
        CodeChunkBuilder { highest_register: 0, instructions: vec![] }
    }
    
    pub fn required_registers(&self) -> u32 {
        self.required_registers
    }
    
    pub fn instructions(&self) -> &[u32] {
        &self.instructions
    }
}

pub struct CodeChunkBuilder {
    highest_register: usize,
    instructions: Vec<u32>
}

impl CodeChunkBuilder {
    pub fn instr<const WORDS: usize>(&mut self, instr: impl Instruction<WORDS>) -> &mut Self {
        let repr = instr.to_bits();
        for register in instr.registers() {
            if register > self.highest_register {
                self.highest_register = register;
            }
        }
        self.instructions.extend_from_slice(&repr);
        self
    }

    pub fn finish(&mut self) -> CodeChunk {
        CodeChunk {
            required_registers: (self.highest_register + 1).try_into().unwrap(),
            instructions: std::mem::take(&mut self.instructions).into_boxed_slice()
        }
    }
}
