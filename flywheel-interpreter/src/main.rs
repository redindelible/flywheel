mod value;

use std::time::Instant;
use crate::value::{UnwrappedValue, Value};

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

    unsafe fn offset(&mut self, amount: isize) {
        unsafe { self.0 = self.0.offset(amount) };
    }
}

struct VMState<'a>(&'a mut VirtualMachine);

impl<'a> VMState<'a> {
    unsafe fn initial_call_frame(self, function: *const Function) -> (InstrPtr, Context<'a>) {
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

    unsafe fn push_call_frame(self, ip: InstrPtr, function: *const Function) -> (InstrPtr, Context<'a>) {
        let current_call_frame = unsafe { self.0.call_stack.last_mut().unwrap_unchecked() };
        current_call_frame.ip = ip;

        unsafe { self.initial_call_frame(function) }
    }

    unsafe fn pop_call_frame(self) -> Option<(InstrPtr, Context<'a>)> {
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

struct Context<'a> {
    registers_view: *mut Value,
    vm_state: VMState<'a>
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

    fn to_vm_state(self) -> VMState<'a> {
        self.vm_state
    }
}


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
        iadd { dst: u8, left: u8, right: u8 },
        imul { dst: u8, left: u8, right: u8 },
        idiv { dst: u8, left: u8, right: u8 },
        imod { dst: u8, left: u8, right: u8 },
        ieq { dst: u8, left: u8, right: u8 },
        jtr { src: u8, off: i16 },
        jmp { _pad: u8, off: i16 },
        ret { src: u8 }
    }
}

type DispatchResult = Result<Value, ()>;

#[cfg(not(feature = "threaded-loop"))]
macro_rules! define {
    { $vis:vis unsafe fn $name:ident($ip:ident: $ip_ty:ty, $frame:ident: $frame_ty:ty) -> $ret:ty { $( $op:ident { $($field:ident),* } => $body:expr )* } } => {
        $vis unsafe fn $name(mut $ip: $ip_ty, mut $frame: $frame_ty) -> $ret {
            let mut instr = unsafe { $ip.get() };
            loop {
                match instr {
                    $(
                        Instruction::$op { $($field),* } => { 
                            $body;
                        }
                    ),*
                }
                instr = unsafe { $ip.advance() };
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
        iadd { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left + right));
        }
        imul { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left * right));
        }
        idiv { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left / right));
        }
        imod { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_i32(left % right));
        }
        ieq { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left).as_int_unchecked();
            let right = frame.read_unchecked(right).as_int_unchecked();
            frame.write_unchecked(dst, Value::from_bool(left == right));
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
    fn execute(&mut self, function: &Function, args: &[Value]) -> Result<Value, ()> {
        assert!(args.len() <= function.required_registers as usize);
        unsafe {
            let (ip, mut frame) = VMState(self).initial_call_frame(function);
            for (i, arg) in args.iter().copied().enumerate() {
                frame.write_unchecked(i, arg)
            }
            execute(ip, frame)
        }
    }
}

fn main() {
    use Instruction::*;

    let function = std::hint::black_box(Function {
        required_registers: 2,
        instructions: vec![
            // Instruction::lzi16 { dst: 0, imm: 3 },
            lzi16 { dst: 1, imm: 0 },

            lzi16 { dst: 2, imm: 1 },
            ieq { dst: 2, left: 0, right: 2 },
            jtr { src: 2, off: 15 },

            lzi16 { dst: 2, imm: 2 },
            imod { dst: 2, left: 0, right: 2 },
            lzi16 { dst: 3, imm: 1 },
            ieq { dst: 2, left: 2, right: 3 },
            jtr { src: 2, off: 3 },

            lzi16 { dst: 2, imm: 2 },
            idiv { dst: 0, left: 0, right: 2 },
            jmp { _pad: 0, off: 4 },

            lzi16 { dst: 2, imm: 3 },
            imul { dst: 0, left: 0, right: 2 },
            lzi16 { dst: 2, imm: 1 },
            iadd { dst: 0, left: 0, right: 2},

            lzi16 { dst: 2, imm: 1 },
            iadd { dst: 1, left: 1, right: 2},
            jmp { _pad: 0, off: -18 },

            ret { src: 1 }
        ].into()
    });

    let mut vm = VirtualMachine::new();

    let start = Instant::now();
    for _ in 0..10000 {
        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        assert!(matches!(result.unwrap().unwrap(), UnwrappedValue::Integer(261)));
    }
    
    let feature = if cfg!(feature = "threaded-loop") {
        "Threaded Loop"
    } else {
        "Match"
    };
    println!("(Using {feature}) Took {:?}", start.elapsed());
}
