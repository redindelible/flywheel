

pub enum Instruction {
    LoadImmI8(i8),
    Add
}

pub struct Decoder<'src> {
    instructions: &'src [u8],
    index: usize
}

impl<'src> Decoder<'src> {
    pub fn new(instructions: &[u8]) -> Decoder {
        Decoder { instructions, index: 0 }
    }

    fn index(&self) -> usize { self.index }
    fn set_index(&mut self, to: usize) { self.index = to; }

    pub fn decode_next(&mut self) -> Instruction {
        let instr = self.instructions[self.index];
        self.index += 1;
        match instr {
            0x00 => {
                let imm = self.instructions[self.index];
                self.index += 1;
                Instruction::LoadImmI8(imm as i8)
            },
            0x01 => Instruction::Add,
            _ => panic!()
        }
    }
}

impl<'src> Iterator for Decoder<'src> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Instruction> {
        Some(self.decode_next())
    }
}

