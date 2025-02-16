use std::fs::File;
use std::io::BufWriter;
use crate::assembly;
use crate::assembly::{BinaryOp, Instruction, Operand, Register, UnaryOp};
use std::io::Write;

pub fn emit(f: File, prog: assembly::Program) -> std::io::Result<()> {
    let mut writer = BufWriter::new(f);

    write!(&mut writer, "\t.globl {}\n", prog.function_definition.name)?;
    write!(&mut writer, "{}:\n", prog.function_definition.name)?;
    write!(&mut writer, "\tpushq  %rbp\n\tmovq   %rsp, %rbp\n")?;

    for inst in prog.function_definition.instructions {
        write!(&mut writer, "\t")?;
        emit_instruction(&mut writer, inst)?;
        write!(&mut writer, "\n")?;
    }

    write!(&mut writer, ".section .note.GNU-stack,\"\",@progbits\n")?;

    writer.flush()?;
    Ok(())
}

fn emit_instruction(writer: &mut BufWriter<File>, inst: assembly::Instruction) -> std::io::Result<()> {
    match inst {
        Instruction::Mov(src, dest) => {
            write!(writer, "movl   ")?;
            emit_operand(writer, src)?;
            write!(writer, ", ")?;
            emit_operand(writer, dest)?;
        },
        Instruction::Ret => {
            write!(writer, "movq   %rbp, %rsp\n\t")?;
            write!(writer, "popq   %rbp\n\t")?;
            write!(writer, "ret")?;
        },
        Instruction::Unary(op, operand) => {
            write!(writer, "{}   ", match op {
                UnaryOp::Not => "notl",
                UnaryOp::Neg => "negl",
            })?;
            emit_operand(writer, operand)?;
        },
        Instruction::AllocateStack(i) => {
            write!(writer, "subq   ${}, %rsp", i)?;
        },
        Instruction::Binary(op, rhs, dst) => {
            write!(writer, "{}   ", match op {
                BinaryOp::Add => "addl",
                BinaryOp::Sub => "subl",
                BinaryOp::Mult => "imull",
                BinaryOp::And => "andl",
                BinaryOp::Or => "orl ",
            })?;
            emit_operand(writer, rhs)?;
            write!(writer, ", ")?;
            emit_operand(writer, dst)?;
        },
        Instruction::Idiv(op) => {
            write!(writer, "idivl   ")?;
            emit_operand(writer, op)?;
        },
        Instruction::Cdq => {
            write!(writer, "cdq")?;
        }
    }

    Ok(())
}

fn emit_operand(writer: &mut BufWriter<File>, op: assembly::Operand) -> std::io::Result<()> {
    match op {
        Operand::Imm(i) => write!(writer, "${}", i)?,
        Operand::Reg(r) => emit_register(writer, r)?,
        Operand::Stack(i) => write!(writer, "-{}(%rbp)", i)?,
        Operand::Pseudo(_) => panic!(),
    }

    Ok(())
}

fn emit_register(writer: &mut BufWriter<File>, reg: assembly::Register) -> std::io::Result<()> {
    match reg {
        Register::AX => write!(writer, "%eax"),
        Register::R10 => write!(writer, "%r10d"),
        Register::R11 => write!(writer, "%r11d"),
        Register::DX => write!(writer, "%edx"),
    }
}