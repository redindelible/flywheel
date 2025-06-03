mod value;
mod instr;
// mod stack;

use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::Arc;
use std::time::Instant;
use crate::instr::{Imm, Instruction, InstructionCode, Reg};
use crate::value::{UnwrappedValue, Value};

struct CodeChunk {
    required_registers: u32,
    instructions: Box<[u32]>
}

impl CodeChunk {
    fn builder() -> CodeChunkBuilder {
        CodeChunkBuilder { highest_register: 0, instructions: vec![] }
    }
}

struct CodeChunkBuilder {
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

// struct CallFrame<'f> {
//     registers_start: usize,
//     ip: InstrPtr<'f>
// }

// struct Context<'vm, 'f> {
//     registers_view: *mut Value,
//     vm_ref: &'vm mut VirtualMachine<'f>
// }
// 
// const _: () = const { assert!(size_of::<Context>() <= 2 * size_of::<usize>()) };
// 
// impl<'vm, 'f> Context<'vm, 'f> {
//     unsafe fn write_unchecked(&mut self, register: impl Into<usize>, value: Value) {
//         unsafe {
//             let register = register.into();
//             self.registers_view.add(register).write(value);
//         }
//     }
// 
//     unsafe fn read_unchecked(&mut self, register: impl Into<usize>) -> Value {
//         unsafe {
//             self.registers_view.add(register.into()).read()
//         }
//     }
// 
//     fn into_inner(self) -> &'vm mut VirtualMachine<'f> {
//         self.vm_ref
//     }
// }

type DispatchResult = Result<Value, ()>;

#[cfg(not(feature = "threaded-loop"))]
macro_rules! define {
    { $vis:vis unsafe fn $name:ident($frame:ident: $frame_ty:ty) -> $ret:ty { $( $op:ident { $($field:ident),* } => $body:expr )* } } => {
        $vis unsafe fn $name(mut $frame: $frame_ty) -> $ret {
            loop {
                let opcode = unsafe { $frame.ip.get_code() };
                match opcode {
                    $(
                        InstructionCode::$op => {
                            let instr::instrs::$op { $($field),* } = unsafe { instr::instrs::$op::from_ptr($frame.ip.0.as_ptr()) };
                            $body;
                        }
                    ),*
                }
                unsafe { $frame.ip.advance() };
            }
        }
    };
}

#[cfg(feature = "threaded-loop")]
macro_rules! define {
    { $vis:vis unsafe fn $name:ident($ip:ident: $ip_ty:ty, $frame:ident: $frame_ty:ty) -> $ret:ty { $( $op:ident { $($field:ident),* } => $body:expr )* } } => {
        const _: () = {
            #[allow(dead_code, unreachable_code, unused_variables)]
            if false {
                match (unreachable!() as Instruction) {
                    $(Instruction::$op { $($field),* } => ()),*
                }
            }
        };

        type DispatchFn = for<'a> unsafe fn(&'a DispatchTable, $ip_ty, $frame_ty) -> $ret;
        struct DispatchTable([Option<DispatchFn>; 256]);
        const DISPATCH_TABLE: DispatchTable = const {
            let mut table: DispatchTable = DispatchTable([None; 256]);
            $(
            table.0[InstructionCode::$op as usize] = Some(|#[allow(unused_variables)] table: &DispatchTable, #[allow(unused_variables, unused_mut)] mut $ip: $ip_ty, #[allow(unused_variables, unused_mut)] mut $frame: $frame_ty| -> $ret {
                let instr = unsafe { $ip.get() };
                let Instruction::$op { $($field),* } = instr else { unsafe { std::hint::unreachable_unchecked() } };
                $body;
                #[allow(unreachable_code)]
                unsafe { dispatch(table, $ip, $frame) }
            });
            )*
            table
        };

        unsafe fn dispatch(table: &DispatchTable, mut instr_ptr: $ip_ty, frame: $frame_ty) -> $ret {
            unsafe {
                let instr = instr_ptr.advance();
                let index = instr.code().bits();
                table.0.get_unchecked(index as usize).unwrap_unchecked()(table, instr_ptr, frame)
            }
        }

        unsafe fn $name($ip: $ip_ty, $frame: $frame_ty) -> $ret {
            unsafe {
                let instr = $ip.get();

                DISPATCH_TABLE.0.get_unchecked(instr.code().bits() as usize).unwrap_unchecked()(&DISPATCH_TABLE, $ip, $frame)
            }
        }
    };
}


define! {
    unsafe fn execute(frame: CallFrame) -> DispatchResult {
        lzi16 { dst, imm } => unsafe {
            frame.write_unchecked(dst.index(), Value::from_i32(imm.as_i32()));
        }
        iaddrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left + right));
        }
        imulrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left * right));
        }
        idivrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left / right));
        }
        imodrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left % right));
        }
        iaddri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left + right.as_i32()));
        }
        imulri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left * right.as_i32()));
        }
        idivri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left / right.as_i32()));
        }
        imodri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left % right.as_i32()));
        }
        ieqrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_bool(left == right));
        }
        ieqri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_bool(left == (right.as_i32())));
        }
        jtr { src, off } => unsafe {
            let bool = frame.read_unchecked(src.index()).as_bool_unchecked();
            if bool {
                frame.ip.offset(off.as_i32() as isize)
            }
        }
        jmp { _pad, off } => unsafe {
            frame.ip.offset(off.as_i32() as isize)
        }
        ret { src, _pad } => unsafe {
            let value = frame.read_unchecked(src.index());
            if let Some(_frame) = frame.pop_call_frame() {
                todo!()
            } else {
                return Ok(value)
            }
        }
    }
}

#[derive(Copy, Clone)]
struct InstrPtr<'f>(NonNull<u32>, PhantomData<&'f u32>);

impl<'f> InstrPtr<'f> {
    fn new(instrs: &'f [u32]) -> Self {
        Self(NonNull::<[u32]>::from(instrs).cast::<u32>(), PhantomData)
    }

    unsafe fn get_code(&self) -> InstructionCode {
        unsafe { self.0.cast::<InstructionCode>().read() }
    }

    unsafe fn advance(&mut self) {
        unsafe {
            self.0 = self.0.byte_add(4);
        }
    }

    unsafe fn offset(&mut self, amount: isize) {
        unsafe { self.0 = self.0.byte_offset(amount * 4) };
    }
}

struct CallFrame<'f> {
    ip: InstrPtr<'f>,
    registers: Box<[Value]>,
    prev: Option<Box<CallFrame<'f>>>
}

impl<'f> CallFrame<'f> {
    unsafe fn write_unchecked(&mut self, register: impl Into<usize>, value: Value) {
        unsafe {
            let register = register.into();
            *self.registers.get_unchecked_mut(register) = value;
        }
    }

    unsafe fn read_unchecked(&mut self, register: impl Into<usize>) -> Value {
        unsafe {
            *self.registers.get_unchecked(register.into())
        }
    }
    
    unsafe fn pop_call_frame(mut self) -> Option<Self> {
        self.prev.take().map(|boxed| *boxed)
    }
}

struct VirtualMachine {
    
}

impl VirtualMachine {
    fn new() -> Self {
        VirtualMachine {
            
        }
    }

    #[inline(never)]
    fn execute<'f>(&mut self, function: &'f Function, args: &[Value]) -> Result<Value, ()> {
        assert!(args.len() <= function.parameters as usize);
        let result = unsafe {
            let mut frame = self.initial_call_frame(function);
            for (i, arg) in args.iter().copied().enumerate() {
                frame.write_unchecked(i, arg)
            }
            execute(frame)
        };
        result
    }

    unsafe fn initial_call_frame<'vm, 'f>(&'vm mut self, function: &'f Function) -> CallFrame<'f> {
        let ip = InstrPtr::new(&*function.code.instructions);
        let required = function.code.required_registers;

        let registers = vec![Value::new_none(); required as usize].into_boxed_slice();

        CallFrame {
            ip,
            registers,
            prev: None
        }
    }
}


struct Function {
    code: Arc<CodeChunk>,
    parameters: u32
}


struct Interpreter {

}


fn main() {
    use instr::instrs::*;

    let code = CodeChunk::builder()
        .instr(lzi16 { dst: Reg(1), imm: Imm(0) })

        .instr(ieqri { dst: Reg(2), left: Reg(0), right: Imm(1) })
        .instr(jtr { src: Reg(2), off: Imm(9) })

        .instr(imodri { dst: Reg(2), left: Reg(0), right: Imm(2) })
        .instr(ieqri { dst: Reg(2), left: Reg(2), right: Imm(1) })
        .instr(jtr { src: Reg(2), off: Imm(2) })

        .instr(idivri { dst: Reg(0), left: Reg(0), right: Imm(2) })
        .instr(jmp { _pad: Reg(0), off: Imm(2) })

        .instr(imulri { dst: Reg(0), left: Reg(0), right: Imm(3) })
        .instr(iaddri { dst: Reg(0), left: Reg(0), right: Imm(1) })

        .instr(iaddri { dst: Reg(1), left: Reg(1), right: Imm(1) })
        .instr(jmp { _pad: Reg(0), off: Imm(-11) })

        .instr(ret { src: Reg(1), _pad: Imm(0) })
        .finish();

    let function = Function { parameters: 1, code: Arc::new(code) };
    
    let mut vm = VirtualMachine::new();

    let start = Instant::now();
    for _ in 0..10000 {
        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 261),
            other => panic!("{:?}", other)
        };
    }
    
    let feature = if cfg!(feature = "threaded-loop") {
        "Threaded Loop"
    } else {
        "Match"
    };
    println!("(Using {feature}) Took {:?}", start.elapsed());
}


#[cfg(test)]
mod test {
    use std::sync::Arc;
    use crate::{CodeChunk, Function, VirtualMachine};
    use crate::instr::{Imm, Reg};
    use crate::value::{UnwrappedValue, Value};

    #[test]
    fn test_collatz() {
        use crate::instr::instrs::*;

        let function = CodeChunk::builder()
            .instr(lzi16 { dst: Reg(1), imm: Imm(0) })
            
            .instr(ieqri { dst: Reg(2), left: Reg(0), right: Imm(1) })
            .instr(jtr { src: Reg(2), off: Imm(9) })
            
            .instr(imodri { dst: Reg(2), left: Reg(0), right: Imm(2) })
            .instr(ieqri { dst: Reg(2), left: Reg(2), right: Imm(1) })
            .instr(jtr { src: Reg(2), off: Imm(2) })
            
            .instr(idivri { dst: Reg(0), left: Reg(0), right: Imm(2) })
            .instr(jmp { _pad: Reg(0), off: Imm(2) })
            
            .instr(imulri { dst: Reg(0), left: Reg(0), right: Imm(3) })
            .instr(iaddri { dst: Reg(0), left: Reg(0), right: Imm(1) })
            
            .instr(iaddri { dst: Reg(1), left: Reg(1), right: Imm(1) })
            .instr(jmp { _pad: Reg(0), off: Imm(-11) })
            
            .instr(ret { src: Reg(1), _pad: Imm(0) })
            .finish();
        
        let function = Function { code: Arc::new(function), parameters: 1 };

        let mut vm = VirtualMachine::new();

        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 261),
            other => panic!("{:?}", other)
        };
    }
}
