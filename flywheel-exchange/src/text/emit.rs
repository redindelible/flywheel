use std::io::{Write, Result};
use crate::interchange as ex;

pub fn pretty_module(writer: &mut impl Write, module: &ex::Module) -> Result<()> {
    for func in &module.functions {
        pretty_function(writer, func)?;
    }

    Ok(())
}

pub fn pretty_function(writer: &mut impl Write, function: &ex::Function) -> Result<()> {
    write!(writer, "def %{} (", &function.name)?;

    let mut is_first = true;
    for param_ty in &function.parameters {
        if !is_first {
            write!(writer, ",")?;
        }
        write_type(writer, param_ty)?;
        is_first = false;
    }

    write!(writer, ") ")?;
    write_type(writer, &function.return_type)?;
    write!(writer, " {{\n")?;

    for (block_id, block) in function.blocks.iter().enumerate() {
        write!(writer, "@{block_id} [")?;
        let mut is_first = true;
        for (local_index, local) in block.retained_locals.iter().enumerate() {
            if !is_first {
                write!(writer, ",")?;
            }
            write_type(writer, local)?;
            write!(writer, " %{local_index}")?;
            is_first = false;
        }
        write!(writer, "]\n")?;

        for instr in &block.instructions {
            write!(writer, "    ")?;
            write_instruction(writer, instr)?;
            write!(writer, "\n")?;
        }

        write!(writer, "    ")?;
        write_terminator(writer, &block.terminator)?;
        write!(writer, "\n")?;
    }

    write!(writer, "}}\n")?;

    Ok(())
}

fn write_type(writer: &mut impl Write, ty: &ex::Type) -> Result<()> {
    match ty {
        ex::Type::Unit => write!(writer, "unit"),
        ex::Type::Bool => write!(writer, "bool"),
        ex::Type::Integer => write!(writer, "int"),
        ex::Type::Float => write!(writer, "float"),
        _ => todo!(),
    }
}

fn write_instruction(writer: &mut impl Write, instr: &ex::Instruction) -> Result<()> {
    match instr {
        ex::Instruction::Pop => write!(writer, "pop"),
        // ex::Instruction::LoadConst { name } => write!(writer, "load.const %{name}"),
        ex::Instruction::LoadLocal { index } => write!(writer, "load.local %{index}"),
        ex::Instruction::StoreLocal { index } => write!(writer, "store.local %{index}"),
        ex::Instruction::LoadUnit => write!(writer, "load.unit"),
        ex::Instruction::LoadTrue => write!(writer, "load.bool true"),
        ex::Instruction::LoadFalse => write!(writer, "load.bool false"),
        ex::Instruction::LoadInteger(number) => write!(writer, "load.int {number}"),
        // ex::Instruction::LoadFloat(number) => write!(writer, "load.float {number}"),
        // ex::Instruction::Upcast => write!(writer, ""),
        // ex::Instruction::Call => write!(writer, ""),
        ex::Instruction::LessEqual => write!(writer, "cmp.le"),
        ex::Instruction::Equal => write!(writer, "cmp.eq"),
        ex::Instruction::Add => write!(writer, "add"),
        ex::Instruction::Mul => write!(writer, "mul"),
        ex::Instruction::Div => write!(writer, "div"),
        ex::Instruction::Mod => write!(writer, "mod"),
    }
}

fn write_terminator(writer: &mut impl Write, term: &ex::Terminator) -> Result<()> {
    match term {
        ex::Terminator::Jump { target } => write!(writer, "jump {}", target.0),
        ex::Terminator::Loop { target } => write!(writer, "loop {}", target.0),
        ex::Terminator::IfElse { true_target, false_target } => write!(writer, "branch {} {}", true_target.0, false_target.0),
        ex::Terminator::Return => write!(writer, "ret"),
    }
}