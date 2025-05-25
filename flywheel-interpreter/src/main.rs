mod value;

use std::time::Instant;
use bytemuck::{Pod, Zeroable};
use crate::value::{UnwrappedValue, Value};

struct Function {
    required_registers: u32,
    instructions: Box<[u32]>,
    constants: Box<[u8]>
}

impl Function {
    fn builder() -> FunctionBuilder {
        FunctionBuilder { highest_register: 0, instructions: vec![], constants: vec![] }
    }
}

struct FunctionBuilder {
    highest_register: u32,
    instructions: Vec<u32>,
    constants: Vec<u8>
}

impl FunctionBuilder {
    pub fn instr(&mut self, instr: impl Instruction) -> &mut Self {
        for register in instr.registers() {
            if register as u32 > self.highest_register {
                self.highest_register = register as u32;
            }
        }
        self.instructions.extend_from_slice(bytemuck::cast_slice(&[instr.to_repr()]));
        self
    }

    pub fn finish(&mut self) -> Function {
        Function {
            required_registers: self.highest_register + 1,
            instructions: std::mem::take(&mut self.instructions).into_boxed_slice(),
            constants: std::mem::take(&mut self.constants).into_boxed_slice(),
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

trait Instruction: Sized {
    type Repr: Pod;

    fn registers(&self) -> impl IntoIterator<Item=u8>;

    fn to_repr(self) -> Self::Repr;
    fn from_repr(repr: Self::Repr) -> Self;

    unsafe fn from_ptr(ptr: *const u32) -> Self {
        Self::from_repr(unsafe { ptr.cast::<Self::Repr>().read() })
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
struct RegImm16 {
    opcode: u8,
    r1: u8,
    imm16: u16
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
struct RegOff16 {
    opcode: u8,
    r1: u8,
    off16: i16
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
struct RegRegReg {
    opcode: u8,
    r1: u8,
    r2: u8,
    r3: u8,
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
struct RegRegImm8 {
    opcode: u8,
    r1: u8,
    r2: u8,
    imm8: i8,
}

macro_rules! _generate_instruction {
    {$instr:ident $opcode:ident r1: $r1:ident, imm16: $imm16:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $imm16: u16
        }


        impl $crate::Instruction for $instr {
            type Repr = $crate::RegImm16;

            fn registers(&self) -> impl IntoIterator<Item=u8> {
                [self.$r1]
            }

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, imm16: self.$imm16 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $imm16: repr.imm16 }
            }
        }
    };
    
    {$instr:ident $opcode:ident r1: $r1:ident, off16: $off16:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $off16: i16
        }


        impl $crate::Instruction for $instr {
            type Repr = $crate::RegOff16;

            fn registers(&self) -> impl IntoIterator<Item=u8> {
                [self.$r1]
            }

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, off16: self.$off16 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $off16: repr.off16 }
            }
        }
    };

    {$instr:ident $opcode:ident r1: $r1:ident, r2: $r2:ident, r3: $r3:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $r2: u8, pub $r3: u8
        }

        impl $crate::Instruction for $instr {
            type Repr = $crate::RegRegReg;

            fn registers(&self) -> impl IntoIterator<Item=u8> {
                [self.$r1, self.$r2, self.$r3]
            }

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, r2: self.$r2, r3: self.$r3 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $r2: repr.r2, $r3: repr.r3 }
            }
        }
    };
    
    {$instr:ident $opcode:ident r1: $r1:ident, r2: $r2:ident, imm8: $imm8:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $r2: u8, pub $imm8: i8
        }

        impl $crate::Instruction for $instr {
            type Repr = $crate::RegRegImm8;

            fn registers(&self) -> impl IntoIterator<Item=u8> {
                [self.$r1, self.$r2]
            }

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, r2: self.$r2, imm8: self.$imm8 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $r2: repr.r2, $imm8: repr.imm8 }
            }
        }
    };
}


macro_rules! generate_instructions {
    { $vis:vis mod $instrs:ident(enum $opcode:ident) { $( $instr:ident { $($field:ident : $ty:ident),* $(,)? } ),* $(,)? } } => {
        $vis mod $instrs {
            $(_generate_instruction! { $instr $opcode $($field: $ty),* })*
        }

        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        #[repr(u8)]
        $vis enum $opcode {
            $($instr),*
        }

        impl $opcode {
            pub fn bits(self) -> u8 {
                self as u8
            }
        }
    };
}

generate_instructions! {
    pub mod instrs(enum InstructionCode) {
        lzi16 { r1: dst, imm16: imm },              // Load integer from 16 bit immediate zero-extended to 32 bits
        iaddrr { r1: dst, r2: left, r3: right },
        imulrr { r1: dst, r2: left, r3: right },
        idivrr { r1: dst, r2: left, r3: right },
        imodrr { r1: dst, r2: left, r3: right },
        iaddri { r1: dst, r2: left, imm8: right },
        imulri { r1: dst, r2: left, imm8: right },
        idivri { r1: dst, r2: left, imm8: right },
        imodri { r1: dst, r2: left, imm8: right },
        ieqrr { r1: dst, r2: left, r3: right },
        ieqri { r1: dst, r2: left, imm8: right },
        jtr { r1: src, off16: off },
        jmp { r1: _pad, off16: off },
        ret { r1: src, imm16: _pad }
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
                            let instrs::$op { $($field),* } = unsafe { instrs::$op::from_ptr($ip.0) };
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
        let result = unsafe {
            let (ip, mut frame) = VMState(self).initial_call_frame(function);
            for (i, arg) in args.iter().copied().enumerate() {
                frame.write_unchecked(i, arg)
            }
            execute(ip, frame)
        };
        assert!(self.registers.is_empty());
        assert!(self.call_stack.is_empty());
        result
    }
}

fn main() {
    use instrs::*;

    let function = Function::builder()
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
    use crate::{Function, VirtualMachine};
    use crate::value::{UnwrappedValue, Value};

    #[test]
    fn test_collatz() {
        use crate::instrs::*;

        let function = Function::builder()
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
