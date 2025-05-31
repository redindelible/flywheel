mod value;
mod instr;

use std::time::Instant;
use crate::instr::{Instruction, InstructionCode, InstructionRepr};
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
    highest_register: u32,
    instructions: Vec<u32>
}

impl CodeChunkBuilder {
    pub fn instr(&mut self, instr: impl Instruction) -> &mut Self {
        let repr = instr.to_repr();
        for register in repr.registers() {
            if register as u32 > self.highest_register {
                self.highest_register = register as u32;
            }
        }
        self.instructions.extend_from_slice(bytemuck::cast_slice(&[repr]));
        self
    }

    pub fn finish(&mut self) -> CodeChunk {
        CodeChunk {
            required_registers: self.highest_register + 1,
            instructions: std::mem::take(&mut self.instructions).into_boxed_slice()
        }
    }
}

struct CallFrame {
    registers_start: usize,
    ip: InstrPtr
}

#[derive(Copy, Clone)]
struct InstrPtr(*const u32);

impl InstrPtr {
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

struct Context<'a> {
    registers_view: *mut Value,
    vm_ref: &'a mut VirtualMachine
}

const _: () = const { assert!(size_of::<Context>() <= 2 * size_of::<usize>()) };

impl<'a> Context<'a> {
    unsafe fn write_unchecked(&mut self, register: impl Into<usize>, value: Value) {
        unsafe {
            let register = register.into();
            self.registers_view.add(register).write(value);
        }
    }

    unsafe fn read_unchecked(&mut self, register: impl Into<usize>) -> Value {
        unsafe {
            self.registers_view.add(register.into()).read()
        }
    }

    fn into_inner(self) -> &'a mut VirtualMachine {
        self.vm_ref
    }
}

type DispatchResult = Result<Value, ()>;

#[cfg(not(feature = "threaded-loop"))]
macro_rules! define {
    { $vis:vis unsafe fn $name:ident($ip:ident: $ip_ty:ty, $frame:ident: $frame_ty:ty) -> $ret:ty { $( $op:ident { $($field:ident),* } => $body:expr )* } } => {
        $vis unsafe fn $name(mut $ip: $ip_ty, mut $frame: $frame_ty) -> $ret {
            loop {
                let opcode = unsafe { $ip.get_code() };
                match opcode {
                    $(
                        InstructionCode::$op => {
                            let instr::instrs::$op { $($field),* } = unsafe { instr::instrs::$op::from_ptr($ip.0) };
                            $body;
                        }
                    ),*
                }
                unsafe { $ip.advance() };
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
    unsafe fn execute(ip: InstrPtr, frame: Context) -> DispatchResult {
        lzi16 { dst, imm } => unsafe {
            frame.write_unchecked(dst, Value::from_i32(imm as u32 as i32));
        }
        iaddrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left + right));
        }
        imulrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left * right));
        }
        idivrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left / right));
        }
        imodrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left % right));
        }
        iaddri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left + (right as i32)));
        }
        imulri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left * (right as i32)));
        }
        idivri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left / (right as i32)));
        }
        imodri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left % (right as i32)));
        }
        ieqrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_bool(left == right));
        }
        ieqri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_bool(left == (right as i32)));
        }
        jtr { src, off } => unsafe {
            let bool = frame.read_unchecked(src).as_bool_unchecked();
            if bool {
                ip.offset(off as isize)
            }
        }
        jmp { _pad, off } => unsafe {
            ip.offset(off as isize)
        }
        ret { src, _pad } => unsafe {
            let value = frame.read_unchecked(src);
            if let Some((_ip, _frame)) = frame.into_inner().pop_call_frame() {
                todo!()
            } else {
                return Ok(value)
            }
        }
    }
}

struct VirtualMachine {
    registers: Vec<Value>,
    call_stack: Vec<CallFrame>,
}

impl VirtualMachine {
    fn new() -> Self {
        VirtualMachine {
            registers: vec![],
            call_stack: vec![]
        }
    }

    #[inline(never)]
    fn execute(&mut self, function: &CodeChunk, args: &[Value]) -> Result<Value, ()> {
        assert!(args.len() <= function.required_registers as usize);
        let result = unsafe {
            let (ip, mut frame) = self.initial_call_frame(function);
            for (i, arg) in args.iter().copied().enumerate() {
                frame.write_unchecked(i, arg)
            }
            execute(ip, frame)
        };
        assert!(self.registers.is_empty());
        assert!(self.call_stack.is_empty());
        result
    }

    unsafe fn initial_call_frame(&mut self, function: *const CodeChunk) -> (InstrPtr, Context<'_>) {
        let ip = unsafe { InstrPtr((*function).instructions.as_ptr()) };
        let required = unsafe { (*function).required_registers };

        let start = self.registers.len();
        self.registers.extend(std::iter::repeat_n(Value::new_none(), required as usize));

        self.call_stack.push(CallFrame {
            registers_start: start,
            ip
        });
        let registers_view = self.registers[start..].as_mut_ptr();
        (ip, Context { registers_view, vm_ref: self })
    }

    unsafe fn push_call_frame(&mut self, ip: InstrPtr, function: *const CodeChunk) -> (InstrPtr, Context<'_>) {
        let current_call_frame = unsafe { self.call_stack.last_mut().unwrap_unchecked() };
        current_call_frame.ip = ip;

        unsafe { self.initial_call_frame(function) }
    }

    unsafe fn pop_call_frame(&mut self) -> Option<(InstrPtr, Context<'_>)> {
        let current = unsafe { self.call_stack.pop().unwrap_unchecked() };
        self.registers.drain(current.registers_start..);
        if let Some(frame) = self.call_stack.last() {
            let start = frame.registers_start;
            let registers_view = self.registers[start..].as_mut_ptr();
            Some((frame.ip, Context { registers_view, vm_ref: self }))
        } else {
            None
        }
    }
}


struct Function {

}


struct Interpreter {

}


fn main() {
    use instr::instrs::*;

    let function = CodeChunk::builder()
        .instr(lzi16 { dst: 1, imm: 0 })

        .instr(ieqri { dst: 2, left: 0, right: 1 })
        .instr(jtr { src: 2, off: 9 })

        .instr(imodri { dst: 2, left: 0, right: 2 })
        .instr(ieqri { dst: 2, left: 2, right: 1 })
        .instr(jtr { src: 2, off: 2 })

        .instr(idivri { dst: 0, left: 0, right: 2 })
        .instr(jmp { _pad: 0, off: 2 })

        .instr(imulri { dst: 0, left: 0, right: 3 })
        .instr(iaddri { dst: 0, left: 0, right: 1 })

        .instr(iaddri { dst: 1, left: 1, right: 1 })
        .instr(jmp { _pad: 0, off: -11 })

        .instr(ret { src: 1, _pad: 0 })
        .finish();

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
    use crate::{CodeChunk, VirtualMachine};
    use crate::value::{UnwrappedValue, Value};

    #[test]
    fn test_collatz() {
        use crate::instr::instrs::*;

        let function = CodeChunk::builder()
            .instr(lzi16 { dst: 1, imm: 0 })
            
            .instr(ieqri { dst: 2, left: 0, right: 1 })
            .instr(jtr { src: 2, off: 9 })
            
            .instr(imodri { dst: 2, left: 0, right: 2 })
            .instr(ieqri { dst: 2, left: 2, right: 1 })
            .instr(jtr { src: 2, off: 2 })
            
            .instr(idivri { dst: 0, left: 0, right: 2 })
            .instr(jmp { _pad: 0, off: 2 })
            
            .instr(imulri { dst: 0, left: 0, right: 3 })
            .instr(iaddri { dst: 0, left: 0, right: 1 })
            
            .instr(iaddri { dst: 1, left: 1, right: 1 })
            .instr(jmp { _pad: 0, off: -11 })
            
            .instr(ret { src: 1, _pad: 0 })
            .finish();

        let mut vm = VirtualMachine::new();

        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 261),
            other => panic!("{:?}", other)
        };
    }
}
