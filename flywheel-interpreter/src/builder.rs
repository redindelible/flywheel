use crate::instr::Instruction;

pub struct CodeChunk {
    pub required_registers: u32,
    pub parameters: u32,
    pub instructions: Box<[u32]>
}

impl CodeChunk {
    pub fn builder(parameters: u32) -> CodeChunkBuilder {
        CodeChunkBuilder { parameters, registers_needed: parameters, instructions: vec![] }
    }
}

pub struct CodeChunkBuilder {
    parameters: u32,
    registers_needed: u32,
    instructions: Vec<u32>
}

impl CodeChunkBuilder {
    pub fn instr<const WORDS: usize>(&mut self, instr: impl Instruction<WORDS>) -> &mut Self {
        let repr = instr.to_bits();
        for register in instr.registers() {
            if register >= self.registers_needed {
                self.registers_needed = register + 1;
            }
        }
        self.instructions.extend_from_slice(&repr);
        self
    }

    pub fn finish(&mut self) -> CodeChunk {
        CodeChunk {
            parameters: self.parameters,
            required_registers: self.registers_needed.try_into().unwrap(),
            instructions: std::mem::take(&mut self.instructions).into_boxed_slice()
        }
    }
}
