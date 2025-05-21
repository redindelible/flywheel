mod value;

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
                self as u8
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

struct Function {
    required_registers: u32,
    instructions: Box<[Instruction]>
}

struct CallFrame {
    registers_start: usize,
    ip: InstrPtr
}

#[derive(Copy, Clone)]
struct InstrPtr(*const Instruction);

impl InstrPtr {
    unsafe fn get(&self) -> Instruction {
        unsafe { *self.0 }
    }

    unsafe fn advance(&mut self) -> Instruction {
        unsafe {
            self.0 = self.0.add(1);
            *self.0
        }
    }
}

struct VMStateInner {
    registers: Vec<Value>,
    call_stack: Vec<CallFrame>,
}

struct VMState(Box<VMStateInner>);

impl VMState {
    fn new() -> Self {
        VMState(Box::new(VMStateInner {
            registers: vec![],
            call_stack: vec![]
        }))
    }

    unsafe fn initial_call_frame(mut self, function: *const Function) -> (InstrPtr, Context) {
        let ip = unsafe { InstrPtr((*function).instructions.as_ptr()) };
        let required = unsafe { (*function).required_registers };

        let start = self.0.registers.len();
        self.0.registers.extend(std::iter::repeat_n(Value::new_none(), required as usize));

        self.0.call_stack.push(CallFrame {
            registers_start: start,
            ip
        });
        let registers_view = self.0.registers[start..].as_mut_ptr();
        (ip, Context { registers_view, vm_state: self })
    }

    unsafe fn push_call_frame(mut self, ip: InstrPtr, function: *const Function) -> (InstrPtr, Context) {
        let current_call_frame = unsafe { self.0.call_stack.last_mut().unwrap_unchecked() };
        current_call_frame.ip = ip;

        unsafe { self.initial_call_frame(function) }
    }

    unsafe fn pop_call_frame(mut self) -> Option<(InstrPtr, Context)> {
        let current = unsafe { self.0.call_stack.pop().unwrap_unchecked() };
        self.0.registers.drain(current.registers_start..);
        if let Some(frame) = self.0.call_stack.last() {
            let start = frame.registers_start;
            let registers_view = self.0.registers[start..].as_mut_ptr();
            Some((frame.ip, Context { registers_view, vm_state: self }))
        } else {
            None
        }
    }
}

struct Context {
    registers_view: *mut Value,
    vm_state: VMState
}

const _: () = const { assert!(size_of::<Context>() <= 2 * size_of::<usize>()) };

impl Context {
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

    fn to_vm_state(self) -> VMState {
        self.vm_state
    }
}

type DispatchResult = Result<Value, ()>;

// macro_rules! define {
//     { $vis:vis unsafe fn $name:ident($ip:ident: $ip_ty:ty, $frame:ident: $frame_ty:ty) -> $ret:ty { $( $op:pat => $body:expr )* } } => {
//         $vis unsafe fn $name(mut $ip: $ip_ty, mut $frame: $frame_ty) -> $ret {
//             let mut instr = unsafe { $ip.get() }; 
//             loop {
//                 match instr {
//                     $(
//                         $op { $(field),* } => $body
//                     ),*
//                 }
//                 instr = unsafe { $ip.advance() }
//             }
//         }
//     };
// }

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
        
        type DispatchFn = unsafe fn(Instruction, $ip_ty, $frame_ty) -> $ret;
        const DISPATCH_TABLE: [Option<DispatchFn>; 256] = const { 
            let mut table: [Option<DispatchFn>; 256] = [None; 256];
            $(
            table[InstructionCode::$op as usize] = Some(|instr, #[allow(unused_variables, unused_mut)] mut $ip: $ip_ty, mut $frame: $frame_ty| -> $ret {
                let Instruction::$op { $($field),* } = instr else { unsafe { std::hint::unreachable_unchecked() } };
                $body;
                #[allow(unreachable_code)]
                unsafe { dispatch($ip, $frame) } 
            });
            )*
            table
        };
        
        unsafe fn dispatch(mut instr_ptr: $ip_ty, frame: $frame_ty) -> $ret {
            unsafe {
                let instr = instr_ptr.advance();
                DISPATCH_TABLE.get_unchecked(instr.code().bits() as usize).unwrap_unchecked()(instr, instr_ptr, frame)
            }
        }
        
        unsafe fn $name($ip: $ip_ty, $frame: $frame_ty) -> $ret {
            unsafe {
                let instr = $ip.get();
        
                DISPATCH_TABLE.get_unchecked(instr.code().bits() as usize).unwrap_unchecked()(instr, $ip, $frame)
            }
        }
    };
}


define! { 
    unsafe fn execute(ip: InstrPtr, frame: Context) -> DispatchResult {
        lzi16 { dst, imm } => unsafe {
            frame.write_unchecked(dst, Value::from_i32(imm as u32 as i32));
        }
        addi { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left + right));
        }
        ret { src } => unsafe {
            let value = frame.read_unchecked(src);
            if let Some((_ip, _frame)) = frame.to_vm_state().pop_call_frame() {
                todo!()
            } else {
                return Ok(value)
            }
        }
    }
}

fn main() {
    let function = Function {
        required_registers: 2,
        instructions: vec![
            Instruction::lzi16 { dst: 0, imm: 3 },
            Instruction::lzi16 { dst: 1, imm: 7 },
            Instruction::addi { dst: 0, left: 0, right: 1 },
            Instruction::ret { src: 0 }
        ].into()
    };

    let context = VMState::new();
    let (ip, frame) = unsafe { context.initial_call_frame(&function) };
    let result = unsafe {
        execute(ip, frame)
    };
    dbg!(result.unwrap().unwrap());
}
