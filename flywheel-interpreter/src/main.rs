mod value;

use std::hint::unreachable_unchecked;
use crate::value::Value;

macro_rules! generate_instructions {
    { $vis:vis enum $instrs:ident(enum $opcode:ident) { $( $instr:ident $({ $($field:ident : $ty:ty),* $(,)? })? ),* $(,)? } } => {
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        #[derive(Copy, Clone)]
        $vis enum $instrs {
            $($instr $({ $($field: $ty),* })?),*
        }
        
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        #[repr(u8)]
        $vis enum $opcode {
            $($instr),*
        }
        
        impl $instrs {
            pub fn code(self) -> $opcode {
                unsafe { (&self as *const $instrs as *const $opcode).read() }
            }
        }
        
        impl $opcode {
            pub fn bits(self) -> u8 {
                unsafe { std::mem::transmute(self) }
            }
        }
    };
}

generate_instructions! {
    pub enum Instruction(enum InstructionCode) {
        lzi16 { dst: u8, imm: u16 },  // Load integer from 16 bit immediate zero-extended to 32 bits
        addi { dst: u8, left: u8, right: u8},
        ret { src: u8 }
    }
}

struct ContextInner {
    registers: Vec<Value>
}

struct Context(Box<ContextInner>);

struct CallFrameAndContext {
    registers_view: *mut Value,
    context: Context
}

impl CallFrameAndContext {
    unsafe fn write_unchecked(&mut self, register: impl Into<usize>, value: Value) {
        unsafe {
            self.registers_view.add(register.into()).write(value);
        }
    }

    unsafe fn read_unchecked(&mut self, register: impl Into<usize>) -> Value {
        unsafe {
            self.registers_view.add(register.into()).read()
        }
    }
    
    fn to_context(self) -> Context {
        self.context
    }
}

type DispatchFn = unsafe fn(Instruction, *const Instruction, CallFrameAndContext) -> DispatchResult;
type DispatchResult = Result<Value, ()>;

macro_rules! dispatch_table {
    [..$d:expr $(, $i:path => $v:expr)* $(,)?] => {
        const {
            let mut arr: [DispatchFn; 256] = [$d; 256];
            #[allow(unreachable_code)]
            if false {
                match (unreachable!() as _) {
                    $($i => ()),*
                }
            }
            $(
            arr[$i as usize] = $v;
            )*
            arr
        }
    };
}

const DISPATCH_TABLE: [DispatchFn; 256] = dispatch_table![
    ..default_instr,
    InstructionCode::lzi16 => dispatch_lzi16,
    InstructionCode::addi => dispatch_addi,
    InstructionCode::ret => dispatch_ret
];

unsafe fn default_instr(instr: Instruction, instr_ptr: *const Instruction, mut frame: CallFrameAndContext) -> DispatchResult {
    unsafe {
        dispatch(instr_ptr, frame)
    }
}

unsafe fn dispatch_lzi16(instr: Instruction, instr_ptr: *const Instruction, mut frame: CallFrameAndContext) -> DispatchResult {
    unsafe {
        let Instruction::lzi16 { dst, imm } = instr else { unreachable_unchecked() };
        frame.write_unchecked(dst, Value::from_i32(imm as u32 as i32));
        
        dispatch(instr_ptr, frame)
    }
}

unsafe fn dispatch_addi(instr: Instruction, instr_ptr: *const Instruction, mut frame: CallFrameAndContext) -> DispatchResult {
    unsafe {
        let Instruction::addi { dst, left, right } = instr else { unreachable_unchecked() };
        let left = frame.read_unchecked(left).as_int_unchecked();
        let right = frame.read_unchecked(right).as_int_unchecked();
        frame.write_unchecked(dst, Value::from_i32(left + right));
        
        dispatch(instr_ptr, frame)
    }
}

unsafe fn dispatch_ret(instr: Instruction, instr_ptr: *const Instruction, mut frame: CallFrameAndContext) -> DispatchResult {
    unsafe {
        let Instruction::ret { src } = instr else { unreachable_unchecked() };
        let value = frame.read_unchecked(src);

        Ok(value)
    }
}

unsafe fn dispatch_start(instr_ptr: *const Instruction, frame: CallFrameAndContext) -> DispatchResult {
    unsafe {
        let instr = *instr_ptr;
        
        DISPATCH_TABLE.get_unchecked(instr.code().bits() as usize)(instr, instr_ptr, frame)
    }
}

unsafe fn dispatch(mut instr_ptr: *const Instruction, mut frame: CallFrameAndContext) -> DispatchResult {
    unsafe {
        instr_ptr = instr_ptr.add(1);
        let instr = *instr_ptr;
        
        DISPATCH_TABLE.get_unchecked(instr.code().bits() as usize)(instr, instr_ptr, frame)
    }
}


fn main() {
    let program = [
        Instruction::lzi16 { dst: 0, imm: 3 },
        Instruction::lzi16 { dst: 1, imm: 7 },
        Instruction::addi { dst: 0, left: 0, right: 1 },
        Instruction::ret { src: 0 }
    ];
    
    let mut context = Context(Box::new(ContextInner { registers: vec![Value::new_none(); 2] }));
    let registers_view = context.0.registers[0..2].as_mut_ptr();
    let frame = CallFrameAndContext { registers_view, context };
    let result = unsafe {
        dispatch_start(program.as_ptr(), frame)
    };
    dbg!(result.unwrap().unwrap());
}
