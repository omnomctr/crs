use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::BufWriter;
use crate::assembly;
use crate::assembly::{BinaryOp, Condition, Instruction, Operand, Register, UnaryOp};
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
            emit_operand(writer, src, 4)?;
            write!(writer, ", ")?;
            emit_operand(writer, dest, 4)?;
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
            emit_operand(writer, operand, 4)?;
        },
        Instruction::AllocateStack(i) => {
            write!(writer, "subq   ${}, %rsp", i)?;
        },
        Instruction::SetCond(cond, op) => {
            write!(writer, "set{}   ",
                cond)?;
            emit_operand(writer, op, 1)?;
        },
        Instruction::Binary(op, rhs, dst) => {
            if op == BinaryOp::LShift || op == BinaryOp::RShift {
                write!(writer, "shll ")?;
                emit_operand(writer, rhs, 1)?;
                write!(writer, ", ")?;
                emit_operand(writer, dst, 4)?;
            } else {
                write!(writer, "{}   ", match op {
                    BinaryOp::Add => "addl",
                    BinaryOp::Sub => "subl",
                    BinaryOp::Mult => "imull",
                    BinaryOp::And => "andl",
                    BinaryOp::Or => "orl ",
                    BinaryOp::Xor => "xorl",
                    BinaryOp::LShift | BinaryOp::RShift => panic!(),
                })?;
                emit_operand(writer, rhs, 4)?;
                write!(writer, ", ")?;
                emit_operand(writer, dst, 4)?;
            }

        },
        Instruction::Idiv(op) => {
            write!(writer, "idivl   ")?;
            emit_operand(writer, op, 4)?;
        },
        Instruction::Cdq => {
            write!(writer, "cdq")?;
        }
        Instruction::Cmp(op1, op2) => {
            write!(writer, "cmpl   ")?;
            emit_operand(writer, op1, 4)?;
            write!(writer, ", ")?;
            emit_operand(writer, op2, 4)?;
        }
        Instruction::Jmp(lbl) => {
            write!(writer, "jmp    .L{}", lbl)?;
        }
        Instruction::JmpCond(cond, lbl) => {
            write!(writer, "j{}    .L{}", cond, lbl)?;
        }
        Instruction::Label(lbl) => {
            write!(writer, ".L{}:", lbl)?;
        }
    }

    Ok(())
}

fn emit_operand(writer: &mut BufWriter<File>, op: assembly::Operand, bytes: u8) -> std::io::Result<()> {
    match op {
        Operand::Imm(i) => write!(writer, "${}", i)?,
        Operand::Reg(r) => emit_register(writer, r, bytes)?,
        Operand::Stack(i) => write!(writer, "-{}(%rbp)", i)?,
        Operand::Pseudo(_) => panic!(),
    }

    Ok(())
}


fn emit_register(writer: &mut BufWriter<File>, reg: assembly::Register, bytes: u8) -> std::io::Result<()> {
    if bytes == 4 { /* DWORD */
        match reg {
            Register::AX => write!(writer, "%eax"),
            Register::R10 => write!(writer, "%r10d"),
            Register::R11 => write!(writer, "%r11d"),
            Register::DX => write!(writer, "%edx"),
            Register::CX => write!(writer, "%ecx"),
        }
    } else if bytes == 1 { /* WORD */
        match reg {
            Register::AX => write!(writer, "%al"),
            Register::R10 => write!(writer, "%r10b"),
            Register::R11 => write!(writer, "%r11b"),
            Register::DX => write!(writer, "%dl"),
            Register::CX => write!(writer, "%cl"),
        }
    } else {
        panic!("unsupported number of bytes specified")
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Condition::Eq => write!(f, "e"),
            Condition::NEq => write!(f, "ne"),
            Condition::GT => write!(f, "g"),
            Condition::GTE => write!(f, "ge"),
            Condition::LT => write!(f, "l"),
            Condition::LTE => write!(f, "le"),
        }
    }
}