use std::fs::File;
use std::io::BufWriter;
use crate::assembly;
use crate::assembly::{Instruction, Operand, Register};
use std::io::Write;

pub fn emit(f: File, prog: assembly::Program) -> std::io::Result<()> {
    let mut writer = BufWriter::new(f);

    write!(&mut writer, "\t.globl {}\n", prog.function_definition.name)?;
    write!(&mut writer, "{}:\n", prog.function_definition.name)?;

    for inst in prog.function_definition.instructions {
        write!(&mut writer, "\t")?;
        emit_instruction(&mut writer, inst)?;
        write!(&mut writer, "\n")?;
    }

    write!(&mut writer, ".section .note.GNU-stack,\"\",@progbits\n")?;

    Ok(())
}

fn emit_instruction(writer: &mut BufWriter<File>, inst: assembly::Instruction) -> std::io::Result<()> {
    match inst {
        Instruction::Mov(src, dest) => {
            write!(writer, "movl  ")?;
            emit_operand(writer, src)?;
            write!(writer, ", ")?;
            emit_operand(writer, dest)?;
        },
        Instruction::Ret => write!(writer, "ret")?,
    }

    Ok(())
}

fn emit_operand(writer: &mut BufWriter<File>, op: assembly::Operand) -> std::io::Result<()> {
    match op {
        Operand::Imm(i) => write!(writer, "${}", i)?,
        Operand::Reg(r) => emit_register(writer, r)?,
    }

    Ok(())
}

fn emit_register(writer: &mut BufWriter<File>, reg: assembly::Register) -> std::io::Result<()> {
    match reg {
        Register::EAX => write!(writer, "%eax"),
    }
}